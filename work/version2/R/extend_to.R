extend_to <- function(by, what = list(default), default) {
  if (!is.list(what)) stop("what should be a list.")
  if (!is.character(by)) stop("by should be a character vector.")
  has_names <- !is.null(names(what))
  if (has_names) {
    if (!all(names(what) %in% by)) {
      wrong <- names(what)[!(names(what) %in% by)]
      wrong <- paste0("'", wrong, "'", collapse = ", ")
      stop("The name(s) ", wrong, " are not present in by")
    }
    res <- vector("list", length(by))
    names(res) <- by
    for (el in names(res)) {
      res[[el]] <- if (is.null(what[[el]])) default else what[[el]]
    }
  } else {
    res <- rep(what, length.out = length(by))
    names(res) <- by
  }
  res
}

