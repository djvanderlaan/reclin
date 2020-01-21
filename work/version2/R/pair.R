
pair <- function(x, y, cluster = NULL, add_xy = TRUE) {
  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)
  if (!missing(cluster) && !is.null(cluster))
    return cluster_pair(cluster, x, y)
  pairs <- data.table::CJ(.x = seq_len(nrow(x)), .y = seq_len(nrow(y)))
  class(pairs) <- c("pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}

