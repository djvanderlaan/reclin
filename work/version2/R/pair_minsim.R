pair_minsim <- function(x, y, on, minsim = 0.0, 
    comparators = list(default_comparator), default_comparator = identical(), 
    cluster = NULL, keep_simsum = TRUE, add_xy = TRUE) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  if (!missing(cluster) && !is.null(cluster))
    return(cluster_pair_minsim(cluster, x, y, on, minsim, comparators, 
      default_comparator))
  comparators <- extend_to(on, comparators, default = default_comparator) 
  ny <- nrow(y)
  nx <- nrow(x)
  max_size <- 1E7
  nchunks <- max(ceiling(nx * ny/ max_size), 1L)
  group <- floor(seq_len(nrow(x))/(nrow(x)+1)*nchunks)
  idx <- split(seq_len(nrow(x)), group)
  pairs <- lapply(idx, function(idx, x, y, on, minsim, comparators) {
    pairs <- CJ(.x = idx, .y = seq_len(nrow(y)))
    pairs[, simsum := rep(0, nrow(pairs))]
    for (var in on) {
      cmp_fun <- comparators[[var]]
      cmp <- cmp_fun(x[pairs$.x, ..var][[1]], y[pairs$.y, ..var][[1]])
      pairs[, simsum := simsum + ..cmp]
    }
    pairs[simsum >= minsim]
  }, x = x, y = y, on = on, minsim = minsim, comparators = comparators)
  pairs <- rbindlist(pairs)
  if (!keep_simsum) pairs <- pairs[, c(".x", ".y")]
  pairs <- structure(pairs, blocking_on = on)
  class(pairs) <- c("pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}

