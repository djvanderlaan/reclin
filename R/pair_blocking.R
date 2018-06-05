#' Generate pairs using simple blocking
#'
#' Generates all combinations of records from \code{x} and \code{y} where the 
#' blocking variables are equal. 
#'
#' @param x first \code{data.frame}
#' @param y second \code{data.frame}
#' @param blocking_var the variables defining the blocks or strata for which 
#'   all pairs of \code{x} and \code{y} will be generated.
#' @param large should the pairs be returned as a \code{\link{ldat}} object.
#' @param add_xy add \code{x} and \code{y} as attributes to the returned 
#'   pairs. This makes calling some subsequent operations that need \code{x} and 
#'   \code{y} (such as \code{\link{compare_pairs}} easier.
#' @param chunk_size used when \code{large = TRUE} to specify the approximate 
#'   number of pairs that are kept in memory.
#'
#' @details
#' Generating (all) pairs of the records of two data sets, is usually the first 
#' step when linking the two data sets. However, this often results in a too 
#' large number of records. Therefore, blocking is usually applied. 
#'  
#' @return 
#' When \code{large} is \code{FALSE}, a \code{data.frame} with two columns, 
#' \code{x} and \code{y}, is returned. Columns \code{x} and \code{y} are 
#' row numbers from \code{data.frame}s \code{x} and \code{y} respectively. 
#' When \code{large} is \code{TRUE}, an object of type \code{ldat} is returned. 
#'
#' @examples
#' data("linkexample1", "linkexample2")
#' pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
#'
#' @import lvec
#' @import ldat
#' @import dplyr
#' @export
pair_blocking <- function(x, y, blocking_var = NULL, large = TRUE, 
    add_xy = TRUE, chunk_size = 1E7) {
  if (missing(chunk_size)) chunk_size <- getOption("chunk_size", chunk_size)
  # when large = TRUE: chunk x; generate pairs for each chunk and append those
  # to the pairs data.frame
  if (large) {
    nblocks <- if (is.null(blocking_var)) 1 else nrow(unique(x[blocking_var]))
    chunks <- chunk(x, chunk_size = round(nblocks * 1E7/(nrow(y)+1)))
    pairs <- ldat(x = lvec(0, type = "numeric"), y = lvec(0, type = "numeric"))
    for (i in seq_along(chunks)) {
      p <- pair_blocking(slice_range(x, range = chunks[[i]], as_r = TRUE), 
        y, blocking_var, large = FALSE)
      p$x <- p$x + chunks[[i]][1] - 1
      pairs <- ldat::append(pairs, p, clone = FALSE)
    }
  } else {
    if (missing(blocking_var) || is.null(blocking_var)) {
      pairs <- expand.grid(x = seq_len(nrow(x)), y = seq_len(nrow(y)))
    } else {
      a <- x[blocking_var]
      a$x <- seq_len(nrow(a))
      b <- y[blocking_var]
      b$y <- seq_len(nrow(b))
      pairs <- dplyr::inner_join(a, b, by = blocking_var) 
      pairs <- pairs[c("x", "y")]
    }
  }
  # Add attributes to pairs
  pairs <- structure(pairs, blocking_var = blocking_var)
  class(pairs) <- c("pairs_blocking", "pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}

