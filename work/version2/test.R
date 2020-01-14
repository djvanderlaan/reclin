
library(data.table)

source("random_data.R")


n <- 10000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


files <- list.files("R", "*.R", full.names = TRUE)
for (file in files) source(file)

# 
# p <- pair_blocking(x, y, blocking_var = "postcode")
# 
# 
# p <- pair_blocking(x, y)
# 
# 
# 
# system.time({
#   vars <- head(names(x), -1)
#   for (var in vars) {
#     p[[var]] <- (x[p$x, ..var] == y[p$y, ..var])[[1]]
#   }
#   tab <- p[, .N, by = vars]
# })
# 
# 
# system.time(
#   tab <- as.data.table(x[p, on = "x"] == y[p, on = "y"])[, .N, by = vars])


library(parallel)

make_pairs <- function(x, y, cl = 2L, name = "foo") {
  if (is.numeric(cl)) {
    cl <- makeCluster(cl)
  }
  clusterEvalQ(cl, library(data.table))
  
  clusterCall(cl, fun = function(name) {
    if (!exists("reclin_env")) reclin_env <<- environment()
    # TODO: warnings are not returned to main
    if (exists(name, envir = reclin_env)) 
      warning("'", name, "' already exists; overwriting.")
    reclin_env[[name]] <- environment()
    TRUE
  }, name = name)
  
  clusterCall(cl, fun = function(name, y) {
    reclin_env[[name]]$y <- as.data.table(y)
    TRUE
  }, name = name, y = y)
  
  group <- floor(seq_len(nrow(x))/(nrow(x)+1)*length(cl))
  x <- split(x, group)
  
  clusterApply(cl, x, function(name, x) {
    reclin_env[[name]]$x <- as.data.table(x)
    TRUE
  }, name = name)
  
  clusterCall(cl, fun = function(name) {
    x <- reclin_env[[name]]$x
    y <- reclin_env[[name]]$y
    reclin_env[[name]]$pairs <- CJ(x = seq_len(nrow(x)), y = seq_len(nrow(y)))
    TRUE
  }, name = name)
  
  list(cl = cl, name = name)
}


compare_pairs <- function(p, on) {
  clusterCall(p$cl, function(name, on) {
    x <- reclin_env[[name]]$x
    y <- reclin_env[[name]]$y
    pairs <- reclin_env[[name]]$pairs
    var <- on[1]
    for (var in on) {
      reclin_env[[name]]$pairs[[var]] <- 
        (x[pairs$x, ..var][[1]] == y[pairs$y, ..var][[1]])
      TRUE
    }  
  }, name = p$name, on = on)
  list(cl = p$cl, name = p$name, on = on)
}


tabulate_pairs <- function(p, on = p$on) {
  tabs <- clusterCall(p$cl, function(name, on) {
    pairs <- reclin_env[[name]]$pairs
    pairs[, .(n = .N), by = on]
  }, name = p$name, on = p$on)
  rbindlist(tabs)[, .(n = sum(n)), by = c(p$on)]
}


system.time({
p <- make_pairs(x, y, cl = 1)

on <- head(names(x), -1)

p <- compare_pairs(p, on = on)

tab <- tabulate_pairs(p)

})



library(reclin)

system.time({

pr <- reclin::pair_blocking(x, y)
pr <- reclin::compare_pairs(pr, by = on)
tabr <- reclin::tabulate_patterns(pr)
})


