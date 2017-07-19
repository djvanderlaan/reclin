

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
#' @examples 
#' x <- sample(10, 1E2, replace = TRUE)
#' y <- sample(8, 1E2, replace = TRUE)
#' w <- rnorm(1E2)
#' sel <- greedy(x, y, w)
#' data.frame(x[sel], y[sel], w[sel])
#' 
#' @useDynLib reclin
#' @export
greedy <- function(x, y, weight) {
  stopifnot(length(x) == length(y))
  stopifnot(length(x) == length(weight))
  o <- order(weight, decreasing = TRUE)
  x <- x[o]
  y <- y[o]
  s <- .Call("greedy_logical_cpp", as.integer(x), as.integer(y), 
    PACKAGE = "reclin")
  s[o] <- s
  s
}

