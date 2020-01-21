
pair_blocking <- function(x, y, on, cluster = NULL, add_xy = TRUE) {
  x <- as.data.table(x)
  y <- as.data.table(y)
  if (!missing(cluster) && !is.null(cluster))
    return cluster_pair_blocking(cluster, x, y, on)
  a <- x[, ..blocking_var]
  a$.x <- seq_len(nrow(a))
  b <- y[, ..blocking_var]
  b$.y <- seq_len(nrow(b))
  pairs <- merge(a, b, by = blocking_var, all.x = FALSE, all.y = FALSE, 
    allow.cartesian = TRUE)
  pairs <- pairs[, c(".x", ".y")]
  pairs <- structure(pairs, blocking_on = on)
  class(pairs) <- c("pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}

