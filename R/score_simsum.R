

#' @export
score_simsum <- function(pairs, var = "simsum", by, add = TRUE,
    na_value = 0, ...) {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("score_simsum")
}


score_simsum.ldat <- function(pairs, var = "simsum", by, add = TRUE,
    na_value = 0, ...) {
  if (missing(by)) by <- attr(pairs, "by")
  if (is.null(by)) 
    stop("Argument 'by' is missing and not in attributes of 'pairs'.")
  simsum <- lvec(nrow(pairs), "numeric")
  for (v in by) {
    val <- 1*pairs[[v]]
    val[is.na(val)] <- na_value
    simsum <- simsum + val
  }
  if (add) { 
    pairs[[var]] <- simsum
    attr(pairs, "score") <- var
    pairs
  } else simsum
}

score_simsum.data.frame <- function(pairs, var = "simsum", by, add = TRUE, 
    na_value = 0, ...) {
  if (missing(by)) by <- attr(pairs, "by")
  if (is.null(by)) 
    stop("Argument 'by' is missing and not in attributes of 'pairs'.")
  simsum <- numeric(nrow(pairs))
  for (v in by) {
    val <- 1*pairs[[v]]
    val[is.na(val)] <- na_value
    simsum <- simsum + val
  }
  if (add) { 
    pairs[[var]] <- simsum
    attr(pairs, "score") <- var
    pairs
  } else simsum
}

