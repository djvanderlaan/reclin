

#' @export
score_simsum <- function(pairs, by, var = "simsum", add = TRUE) {
  if (missing(by)) by <- attr(pairs, "by")
  if (is.null(by)) 
    stop("Argument 'by' is missing and not in attributes of 'pairs'.")
  score_simsum_impl(pairs, by, var, add)
}


score_simsum_impl <- function(pairs, by, var, add, ...) {
  UseMethod("score_simsum_impl")
}

score_simsum_impl.ldat <- function(pairs, by, var, add, ...) {
  simsum <- lvec(nrow(pairs), "numeric")
  for (v in by) {
    val <- clone(pairs[[v]])
    val[is.na(val)] <- 0
    simsum <- simsum + val
  }
  if (add) { 
    pairs[[var]] <- simsum
    pairs
  } else simsum
}

score_simsum_impl.data.frame <- function(pairs, by, var, add, ...) {
  simsum <- numeric(nrow(pairs))
  for (v in by) {
    val <- pairs[[v]]
    val[is.na(val)] <- 0
    simsum <- simsum + val
  }
  if (add) { 
    pairs[[var]] <- simsum
    pairs
  } else simsum
}

