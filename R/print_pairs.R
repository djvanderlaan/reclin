


big_num <- function(x, ...) {
  format(x, big.mark = " ", ...)
}


#' @import ldat
#' @import lvec
#' @export
print.pairs <- function(x, ...) {
  d_x <- attr(x, "x")
  d_y <- attr(x, "y")
  if (!is.null(d_x)) 
    cat("  First data set:  ", big_num(nrow(d_x)), " records\n", sep = "")
  if (!is.null(d_y)) 
    cat("  Second data set: ", big_num(nrow(d_y)), " records\n", sep = "")
  cat("  Total number of pairs: ", big_num(nrow(x)), " pairs\n", sep = "")
  
  if (is_ldat(x)) {
    cat("\n")
    NextMethod(x)
  } else {
    if (nrow(x) <= 20) {
      cat("\nShowing all pairs:\n") 
      print.data.frame(x)
    } else {
      cat("\nShowing first 20 pairs:\n")
      print.data.frame(utils::head(x, 20))
    }
    
  }
  invisible(x)
}

#' @export
print.pairs_blocking <- function(x, ...) {
  cat("Simple blocking\n")
  if (!is.null(attr(x, "blocking_var"))) {
    cat("  Blocking variable(s): ", 
      paste(attr(x, "blocking_var"), collapse=", "), "\n", sep = "")
  } else {
    cat("  No blocking used.\n")
  }
  NextMethod()
}

#' @export
print.compare <- function(x, ...) {
  cat("Compare\n")
  cat("  By: ", paste(attr(x, "by"), collapse = ", "), "\n\n", sep = "")
  NextMethod()
}
