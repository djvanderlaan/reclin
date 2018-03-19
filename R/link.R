
#' @export
link <- function(pairs, selection = NULL, x = NULL, y = NULL, all_x = TRUE, 
    all_y = TRUE, ...) {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("link")
}


link.data.frame <- function(pairs, selection = NULL, x = NULL, y = NULL,
    all_x = TRUE, all_y = TRUE, ...) {
  link_impl(pairs, selection, x, y, all_x, all_y)
}

link.ldat <- function(pairs, selection = NULL, x = NULL, y = NULL, all_x = TRUE, 
    all_y = TRUE, ...) {
  link_impl(pairs, selection, x, y, all_x, all_y)
}


link_impl <- function(pairs, selection = NULL, x = NULL, y = NULL, 
    all_x = TRUE, all_y = TRUE) {
  # Process x and y
  if (missing(x) || is.null(x)) x <- attr(pairs, "x")
  if (is.null(x)) stop("Missing x")
  if (missing(y) || is.null(y)) y <- attr(pairs, "y")
  if (is.null(y)) stop("Missing y")
  x$.x <- seq_len(nrow(x))
  y$.y <- seq_len(nrow(y))
  # Process selection
  if (missing(selection) || is.null(selection)) 
    selection <- attr(pairs, "selection")
  # (repeat previous if.. in case attribute was not set)
  if (missing(selection) || is.null(selection)) {
    selection <- TRUE
  } else if (is.character(selection)) {
    if (length(selection) != 1) 
      stop("When selection is a character vector; it needs to be length 1.")
    selection <- pairs[[selection]]
  } 
  # Link
  res <- data.frame(.x = as_rvec(pairs$x[selection]),
    .y = as_rvec(pairs$y[selection]))
  res <- if (all_x) dplyr::full_join(res, x, by = ".x") else 
    dplyr::left_join(res, x, by = ".x")
  res <- if (all_y) dplyr::full_join(res, y, by = ".y") else 
    dplyr::left_join(res, y, by = ".y") 
  dplyr::select(res, -.x, -.y)
}
