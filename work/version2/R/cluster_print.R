
#' @export
print.cluster_pairs <- function(x, ...) {
  res <- clusterCall(clpairs$cluster, function(name) {
    env <- reclin_env[[name]]
    pairs <- env$pairs
    nx <- nrow(attr(pairs, "x"))
    ny <- nrow(attr(pairs, "y"))
    np <- nrow(pairs)
    
    pairs <- pairs[sample(nrow(pairs), min(nrow(pairs), 5)), ]
    x <- attr(pairs, "x")
    pairs$.x <- x$.id[pairs$.x]
    
    pairs
      
    list(nx = nx, ny = ny, np = np, pairs = pairs)
  }, name = clpairs$name)
  
  pairs <- rbindlist(lapply(res, function(d) d$pairs))
  nx <- sum(sapply(res, function(d) d$nx))
  ny <- head(sapply(res, function(d) d$ny), 1)
  np <- sum(sapply(res, function(d) d$np))
  
  cat("  Cluster '", x$name, "' with size: ", length(x$cluster), "\n", sep = "")
  cat("  First data set:  ", big_num(nx), " records\n", sep = "")
  cat("  Second data set: ", big_num(ny), " records\n", sep = "")
  cat("  Total number of pairs: ", big_num(np), " pairs\n", sep = "")
  if (!is.null(attr(x, "blocking_on"))) {
    on <- strlist(attr(x, "blocking_on"), indent = "    ")
    cat("  Blocking on: ", on, "\n", sep = "")
  }
  if (!is.null(attr(x, "compare_on"))) {
    on <- strlist(attr(x, "compare_on"), indent = "    ")
    cat("  Comparing on: ", on, "\n", sep = "")
  }
  cat("\nShowing a random selection of pairs:\n")
  print(pairs)
  invisible(x)
}

