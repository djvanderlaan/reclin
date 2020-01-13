
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





