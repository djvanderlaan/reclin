

#' @export
print.pairs <- function(x, ...) {
  d_x <- attr(x, "x")
  d_y <- attr(x, "y")
  if (!is.null(d_x)) 
    cat("  First data set:  ", big_num(nrow(d_x)), " records\n", sep = "")
  if (!is.null(d_y)) 
    cat("  Second data set: ", big_num(nrow(d_y)), " records\n", sep = "")
  cat("  Total number of pairs: ", big_num(nrow(x)), " pairs\n", sep = "")
  NextMethod(x)
  invisible(x)
}



big_num <- function(x, ...) {
  format(x, big.mark = " ", ...)
}


