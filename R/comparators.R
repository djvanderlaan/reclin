
#' @rdname comparators
#' @export
identical <- function(x, y) {
  if (missing(x) && missing(y)) return(identical)
  if (!missing(y)) {
    x == y
  } else {
    x & !is.na(x)
  }
}

#' @rdname comparators
#' @export
jaro_winkler <- function(threshold = 0.95) {
  function(x, y) {
    if (!missing(y)) {
      1-stringdist::stringdist(x, y, method = "jw")
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}
