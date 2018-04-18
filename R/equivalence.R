
#' @useDynLib reclin
equivalence <- function(x, rules) {
  tmp <- unique(x)
  tmp <- tmp[!is.na(tmp)]
  a <- match(rules[[1]], tmp)
  b <- match(rules[[2]], tmp)
  sel <- !is.na(a) & !is.na(b)
  if (any(!sel)) {
    warning("Not all values in rules are present in x.")
    a <- a[sel]
    b <- b[sel]
  }
  res <- equivalence_rcpp(as.integer(a)-1L, as.integer(b)-1L, length(tmp))
  res[match(x, tmp)] + 1L
}

