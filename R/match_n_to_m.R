
#' Force n to m matching on a set of pairs
#' 
#' @param x a vector of identifiers for each x in each pair This vector should 
#'   have a unique value for each element in x.
#' @param y a vector of identifiers for each y in each pair This vector should 
#'   have a unique value for each element in y.
#' @param w a vector with weights for each pair. The algorithm will try to 
#'   maximise the total weight of the selected pairs. 
#' @param n an integer. Each element of x can be linked to at most n elements of
#'   y. 
#' @param m an integer. Each element of y can be linked to at most m elements of
#'   y. 
#'
#' @details
#' The algorithm will try to select pairs in such a way each element of \code{x}
#' is matched to at most \code{n} elements of \code{y} and that each element of
#' \code{y} is matched at most \code{m} elements of \code{x}. It tries to select 
#' elements in such a way that the total weight \code{w} of the selected 
#' elements is maximised. 
#' 
#' @examples
#' d <- data.frame(x=c(1,1,1,2,2,3,3), y=c(1,2,3,4,5,6,7), w=1:7)
#' # One-to-one matching:
#' d[match_n_to_m(d$x, d$y, d$w), ]
#' 
#' # N-to-one matching:
#' d[match_n_to_m(d$x, d$y, d$w, n=999), ]
#' 
#' # One-to-m matching:
#' d[match_n_to_m(d$x, d$y, d$w, m=999), ]
#' 
#' # N-to-M matching, e.g. select all pairs
#' d[match_n_to_m(d$x, d$y, d$w, n=999, m=999), ]
#' 
#' @importFrom lpSolve lp
#' @export
match_n_to_m <- function(x, y, w, n = 1, m = 1) {
  stopifnot(length(x) == length(y))
  stopifnot(length(x) == length(w))
  stopifnot(length(n) == 1 && as.integer(n) == n)
  stopifnot(length(m) == 1 && as.integer(m) == m)
  stopifnot(!(anyNA(x) || anyNA(y) || anyNA(w)))
  if (length(x) == 0) return(integer(0))
  
  d  <- data.frame(x=as.numeric(as.factor(x)), y=as.numeric(as.factor(y)), w=w)
  nx <- length(unique(d$x))
  ny <- length(unique(d$y))
  C  <- cbind(c(d$x, d$y + nx), seq_len(nrow(d)))
  C  <- cbind(C, 1)
  res <- lpSolve::lp("max", d$w, dense.const = C, const.dir = rep("<=", nx+ny), 
    const.rhs = c(rep(n, nx), rep(m, ny)), all.bin=TRUE, use.rw=TRUE)  
  if (res$status != 0) warning("No solution found.")
  which(res$solution > 0)
}
