

#' Greedy one-to-one matching of pairs
#' 
#' @param x id's of lhs of pairs
#' @param y id's of rhs of pairs
#' @param weight weight of pair
#' 
#' @details 
#' Pairs with the highest weight are selected as long a neither the lhs as the
#' rhs are already selected in a pair with a higher weight. 
#' 
#' 
#' @useDynLib reclin
#' @import Rcpp
#' @importFrom Rcpp evalCpp
greedy <- function(x, y, weight) {
  stopifnot(length(x) == length(y))
  stopifnot(length(x) == length(weight))
  o <- order(weight, decreasing = TRUE)
  x <- x[o]
  y <- y[o]
  s <- greedy_rcpp(x, y)
  s[o] <- s
  s
}
