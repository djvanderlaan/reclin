
library(data.table)

source("random_data.R")


n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


files <- list.files("R", "*.R", full.names = TRUE)
for (file in files) source(file)


p <- pair_blocking(x, y, blocking_var = "postcode")


p <- pair_blocking(x, y)



system.time({
  vars <- head(names(x), -1)
  for (var in vars) {
    p[[var]] <- (x[p$x, ..var] == y[p$y, ..var])[[1]]
  }
  tab <- p[, .N, by = vars]
})


system.time(
  tab <- as.data.table(x[p, on = "x"] == y[p, on = "y"])[, .N, by = vars])




library(parallel)

cl <- makeCluster(4)
clusterEvalQ(cl, library(data.table))

group <- floor(seq_len(nrow(x))/(nrow(x)+1)*4)



clusterExport(cl, "y")

x <- split(x, group)
parLapply(cl, x, function(x) {x <<- x; NULL})

clusterExport(cl, "pair_blocking")
clusterEvalQ(cl, p <<- pair_blocking(x, y))

clusterExport(cl, "vars")

system.time({
clusterEvalQ(cl, {
  for (var in vars) {
    p[[var]] <- (x[p$x, ..var] == y[p$y, ..var])[[1]]
  }
})

tab <- clusterEvalQ(cl, p[, .N, by = vars])
})

