
cluster_collect <- function(pairs, clear = FALSE) {
  # Collect pairs
  tmp <- clusterCall(pairs$cluster, function(name, clear) {
    env <- reclin_env[[name]]
    pairs <- env$pairs
    x <- attr(pairs, "x")
    pairs$.x <- x$.id[pairs$.x]
    if (clear) {
      env$pairs <- NULL
      gc()
    }
    pairs
  }, name = pairs$name, clear = clear)
  p <- rbindlist(tmp)
  # x has been split; combine again into one dataset and add to pairs
  x <- lapply(tmp, function(d) attr(d, "x"))
  x <- rbindlist(x)
  attr(p, "x") <- x
  # Build a list of attributes we want to copy from the data.tables in tmp to 
  # the new pairs list
  attr_ignore <- c("x", "names", "row.names", "class")
  attr_names <- names(attributes(tmp[[1]]))
  attr_names <- setdiff(attr_names, attr_ignore)
  attr_names <- attr_names[!grepl("^\\.", attr_names)]
  # Copy/set attributes
  for (name in attr_names) {
    attr(p, name) <- attr(tmp[[1]], name)
  }
  class(p) <- c("pairs", class(p))
  p
}

