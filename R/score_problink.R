
#' @export
score_problink <- function(pairs, model = NULL, var = "weight", 
    add = TRUE, ...) {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("score_problink")
}

score_problink.data.frame <- function(pairs, model = NULL, var = "weight", 
    add = TRUE, ...) {
  if (missing(var)) {
    score_problink_impl(pairs, model, NULL, add, ...)
  } else { 
    score_problink_impl(pairs, model, var, add, ...)
  }
}

score_problink.ldat <- function(pairs, model = NULL, var = "weight", 
    add = TRUE, ...) {
  if (missing(var)) {
    score_problink_impl(pairs, model, NULL, add, ...)
  } else { 
    score_problink_impl(pairs, model, var, add, ...)
  }
}

score_problink_impl <- function(pairs, model = NULL, var = "weight", add, ...) {
  if (missing(model) || is.null(model)) model <- problink_em(pairs)
  p <- predict(model, newdata = pairs, ...)
  if (!add) return(p)
  if (!is.data.frame(p) && !is_ldat(p)) {
    if (is.null(var)) var <- "weight"
    pairs[[var]] <- p
    attr(pairs, "score") <- var
  } else {
    prepend <- if (!missing(var) && !is.null(var)) paste0(var, "_") else ""
    names(p) <- paste0(prepend, names(p))
    for (col in names(p)) pairs[[col]] <- p[[col]]
    if (paste0(prepend, "weight") %in% names(p)) {
      attr(pairs, "score") <- paste0(prepend, "weight")
    } else if (paste0(prepend, "mpost") %in% names(p)) {
      attr(pairs, "score") <- paste0(prepend, "mpost")
    }
  }
  pairs
}

