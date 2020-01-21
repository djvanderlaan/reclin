
library(data.table)

source("random_data.R")


n <- 5000
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
  idx <- split(seq_len(nrow(x)), group)
  x <- split(x, group)
  
  clusterApply(cl, x, function(name, x) {
    reclin_env[[name]]$x <- as.data.table(x)
    TRUE
  }, name = name)
  
  clusterApply(cl, idx, function(name, idx) {
    reclin_env[[name]]$idx <- idx
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

collect <- function(p) {
  pairs <- clusterCall(p$cl, function(name) {
    pairs <- reclin_env[[name]]$pairs
    idx <- reclin_env[[name]]$idx
    pairs$x <- idx[pairs$x]
    pairs
  }, name = p$name)
  rbindlist(pairs)
}

system.time({
p <- make_pairs(x, y, cl = 2)

on <- head(names(x), -1)

p <- compare_pairs(p, on = on)



tab <- tabulate_pairs(p)

})



x
y

pairs <- CJ(x = seq_len(nrow(x)), y = seq_len(nrow(y)))
nrow(pairs)/1E6

on <- head(names(x), -1)
var <- on[1]

system.time({
  res <- x[pairs$x, ..var][[1]] == y[pairs$y, ..var][[1]]
})

system.time({
  xi <- x[pairs$x, ..var][[1]]
})

system.time({
  yi <- y[pairs$y, ..var][[1]]
})

system.time({
  res <- xi == yi
})

identical <- function(x, y) {
  x == y
}

t <- system.time({
  res <- identical(x[pairs$x, ..var][[1]], y[pairs$y, ..var][[1]])
})
t[3]/nrow(pairs)*1E6


library(stringdist)



t <- system.time({
  res <- stringdist::stringsim(x[pairs$x, ..var][[1]], y[pairs$y, ..var][[1]], 
    method = "jw")
})


t[3]/nrow(pairs)*1E6

# 
# library(reclin)
# 
# system.time({
# 
# pr <- reclin::pair_blocking(x, y)
# pr <- reclin::compare_pairs(pr, by = on)
# tabr <- reclin::tabulate_patterns(pr)
# })
# 
# 



# Indentiy 0.054 sec per miljoen records
# Jaro-Winkler 0.9 sec per miljoen records
# Neem aan dat dataset ongeveer 8 kolommen heeft waarop gekoppeld wordt
# Helft daarvan met stringdist helft daarvan met identity
ncol <- 8
time_per_record <- (8*0.5*0.05 + 8*0.5*0.9)/1E6
n <- seq(0, 20E6, by =  1000)
# Total time without blocking
t <- n^2 * time_per_record / 3600
plot(n, t)
# Ga uit van het aantal paren
n <- exp(seq(0, log(1E12), length.out = 100))
n <- seq(0, 1E12, length.out = 100)
t <- n * time_per_record / 3600
plot(n, t, log='xy')
abline(h = 24)

min(n[t > 24])/1E10

sqrt(1E10)


# Which options do I want when generating pairs:
# - blocking
# - only generate pairs with a simsum > X
# - generate all pairs (but do not store) and tabulate comparison patterns

# Last two options only option for limited dataset sizes
sqrt(24*3600/(time_per_record))
# In 24 hours computation time one can run approx 150.000x150.000 linkage
# For second options (simsum > X) one can use identity comparison perhaps using 
# only a limited number of variables
time_per_record2 <- (8*1*0.05 + 8*0*0.9)/1E6
sqrt(24*3600/(time_per_record2))
# Then 0.5E6 x 0.5E6 e.g. a factor sqrt(0.9/0.05) higher






