
library(data.table)
library(stringdist)

source("random_data.R")

n <- 10000
dta <- random_data(n1 = n, n2 = n*0.8, overlap = 0.2)

x <- as.data.table(dta[[1]])
y <- as.data.table(dta[[2]])


files <- list.files("R", "*.R", full.names = TRUE)
for (file in files) source(file)


library(parallel)
cl <- makeCluster(4)

pairs <- pair(x, y)

pairs <- pair_blocking(x, y, on = c("postcode"))

system.time({
  pairs <- pair_minsim(x, y, on = names(x)[1:8], minsim = 2)
  nrow(pairs)
})

system.time({
  clpairs <- cluster_pair_minsim(cluster = cl, x, y, on = names(x)[1:8], minsim = 2)
  sum(unlist(clusterEvalQ(cl, nrow(reclin_env[["default"]]$pairs))))
})

clusterEvalQ(cl, attr(reclin_env[["default"]]$pairs, "x"))


system.time({
pairs <- compare_pairs(pairs, on = names(x)[1:8], comparators = list(
  last_name = jaro_winkler(),
  street = jaro_winkler()
))
})

system.time({
compare_pairs(clpairs, on = names(x)[1:8], comparators = list(
  last_name = jaro_winkler(),
  street = jaro_winkler()
))
})



# foo <- compare_pairs(clpairs, on = names(x)[1:4], comparators = list(
#   last_name = jaro_winkler(),
#   street = jaro_winkler()
# ), new_name = "foo", overwrite = TRUE)


clusterEvalQ(cl, head(reclin_env[["default"]]$pairs))





tab <- tabulate_patterns(pairs)


on <- attr(pairs, "compare_on")
possible_values <- lapply(tab[, ..on], function(x) {
  u <- unique(x)
  if (is.factor(x)) u <- union(x, levels(x))
  u
})
i <- do.call(CJ, possible_values)
tab <- tab[i, , on = on]
tab[is.na(n), n:= 0]



