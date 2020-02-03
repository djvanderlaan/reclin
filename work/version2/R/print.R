

#' @export
print.pairs <- function(x, ...) {
  d_x <- attr(x, "x")
  d_y <- attr(x, "y")
  if (!is.null(d_x)) 
    cat("  First data set:  ", big_num(nrow(d_x)), " records\n", sep = "")
  if (!is.null(d_y)) 
    cat("  Second data set: ", big_num(nrow(d_y)), " records\n", sep = "")
  cat("  Total number of pairs: ", big_num(nrow(x)), " pairs\n", sep = "")
  if (!is.null(attr(x, "blocking_on"))) {
    on <- strlist(attr(x, "blocking_on"), indent = "    ")
    cat("  Blocking on: ", on, "\n", sep = "")
  }
  if (!is.null(attr(x, "compare_on"))) {
    on <- strlist(attr(x, "compare_on"), indent = "    ")
    cat("  Comparing on: ", on, "\n", sep = "")
  }
  cat("\n")
  NextMethod(x)
  invisible(x)
}



big_num <- function(x, ...) {
  format(x, big.mark = " ", ...)
}



strlist <- function(x, indent = "    ") {
  x <- paste0("'", x, "'")
  nchar <- nchar(x)+2
  nlines <- ceiling(sum(nchar)/75)
  line <- floor(cumsum(nchar)/sum(nchar)*nlines*0.99999)+1
  paste0(sapply(split(x, line), paste0, collapse = ", "), 
    collapse=paste0(",\n", "    "))
}
