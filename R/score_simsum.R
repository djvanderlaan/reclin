

#' @export
score_simsum <- function(pairs, by, add = TRUE) {
  if (missing(by)) by <- attr(pairs, "by")
  if (is.null(by)) 
    stop("Argument 'by' is missing and not in attributes of 'pairs'.")
  score_simsum_impl(pairs, by)

}


score_simsum_impl <- function(pairs, by, add, ...) {
  UseMethod("score_simsum_impl")
}

score_simsum_impl.ldat <- function(pairs, by, add, ...) {
  simsum <- lvec(nrow(pairs), "numeric")
  for (var in by) {
    val <- clone(pairs[[var]])
    val[is.na(val)] <- 0
    simsum <- simsum + val
  }
    if (add) { 
    pairs$score = simsum
    pairs
  } else simsum
}

score_simsum_impl.data.frame <- function(pairs, by, add, ...) {
  simsum <- numeric(nrow(pairs))
  for (var in by) {
    val <- pairs[[var]]
    val[is.na(val)] <- 0
    simsum <- simsum + val
  }
  if (add) { 
    pairs$score = simsum
    pairs
  } else simsum
}

