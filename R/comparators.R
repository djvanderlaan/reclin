
#' Comparison functions
#' 
#' @param threshold threshold to use for the Jaro-Winkler string distance when
#'   creating a binary result.
#'   
#' @details 
#' A comparison function should accept two arguments: both vectors. When the 
#' function is called with both arguments it should compare the elements in the 
#' first vector to those in the second. When called in this way, both vectors
#' have the same length. What the function should return depends on the methods
#' used to score the pairs. Usually the comparison functions return a similarity
#' score with a value of 0 indication complete difference and a value > 0 
#' indicating similarity (often a value of 1 will indicate perfect similarity). 
#' 
#' Some methods, such a \code{\link{score_problink}} and 
#' \code{\link{problink_em}}, can handle similarity scores, but also need 
#' binary values (\code{0}/\code{FALSE} = complete dissimilarity; 
#' \code{1}/\code{TRUE} = complete similarity). In order to allow for this the
#' comparison function is called with one argument.
#' 
#' When the comparison is called with one argument, it is passed the result of
#' a previous comparison. The function should translate that result to a binary 
#' (\code{TRUE}/\code{FALSE} or \code{1}/\code{0}) result. The result should 
#' not contain missing values. 
#' 
#' The `jaro_winkler`, `lcs` and `jaccard` functions use the corresponding 
#' methods from \code{\link{stringdist}} except that they are transformed from
#' a distnce to a similarity score.
#' 
#' @return 
#' The functions return a comparison function (see details).
#' 
#' @examples 
#' cmp <- identical()
#' x <- cmp(c("john", "mary", "susan", "jack"), 
#'          c("johan", "mary", "susanna", NA))
#' # Applying the comparison function to the result of the comparison results 
#' # in a logical result, with NA's and values of FALSE set to FALSE
#' cmp(x)
#' 
#' cmp <- jaro_winkler(0.95)
#' x <- cmp(c("john", "mary", "susan", "jack"), 
#'          c("johan", "mary", "susanna", NA))
#' # Applying the comparison function to the result of the comparison results 
#' # in a logical result, with NA's and values below the threshold FALSE
#' cmp(x)
#' 
#' @rdname comparators
#' @export
identical <- function() {
  function(x, y) {
    if (is.factor(x) || (!missing(y) && is.factor(y))) {
      x <- as.character(x)
      y <- as.character(y)
    }
    if (!missing(y)) {
      x == y
    } else {
      x & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @importFrom stringdist stringdist
#' @export
jaro_winkler <- function(threshold = 0.95) {
  function(x, y) {
    if (!missing(y)) {
      1-stringdist(x, y, method = "jw")
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @importFrom stringdist stringdist
#' @export
lcs <- function(threshold = 0.80) {
  function(x, y) {
    if (!missing(y)) {
      d <- stringdist(x, y, method = "lcs")
      maxd <- nchar(x) + nchar(y)
      1 - d/maxd
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}

#' @rdname comparators
#' @importFrom stringdist stringdist
#' @export
jaccard <- function(threshold = 0.80) {
  function(x, y) {
    if (!missing(y)) {
      1-stringdist(x, y, method = "jaccard", q = 2)
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}
