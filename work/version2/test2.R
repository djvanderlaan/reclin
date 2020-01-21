
library(data.table)
source("random_data.R")



pair <- function(x, y, on, min_dist = 0.5) {
  ny <- nrow(y)
  nx <- nrow(x)
  max_size <- 1E7
  nchunks <- max(ceiling(nx * ny/ max_size), 1L)
  group <- floor(seq_len(nrow(x))/(nrow(x)+1)*nchunks)
  idx <- split(seq_len(nrow(x)), group)
  
  pairs <- lapply(idx, function(idx, x, y, on, min_dist) {
    pairs <- CJ(x = idx, y = seq_len(nrow(y)))
    pairs[, simsum := rep(0, nrow(pairs))]
    for (var in on) {
      cmp <- (x[pairs$x, ..var][[1]] == y[pairs$y, ..var][[1]])
      pairs[, simsum := simsum + ..cmp]
    }
    pairs[simsum >= min_dist]
  }, x = x, y = y, on = on, min_dist = min_dist)
  rbindlist(pairs)
}



n <- 5000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2)
x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])

on <- head(names(x), -1)

pair(x, y, on = on)





n <- exp(seq(log(1E3), log(1E4), length.out = 10))
t <- numeric(length(n))
s <- numeric(length(n))

for (i in seq_along(n)) {
  message(Sys.time(), " - ", i, "/", length(n))
  dta <- random_data(n1 = n[i], n2 = n[i]*0.8, overlap = 0.2)
  x <- as.data.table(dta[[1]])
  y <- as.data.table(dta[[2]])
  t[i] <- system.time({
    pairs <- pair(x, y, on = on, min_dist = 2.5)
  })[3]
  s[i] <- object.size(pairs)
}




# Mogelijkheden
