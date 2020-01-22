

compare_pairs <- function(pairs, on, comparators = list(default_comparator), 
    default_comparator = identical(), ...) {
  UseMethod("compare_pairs", pairs)
}


compare_pairs.pairs <- function(pairs, on, 
    comparators = list(default_comparator), default_comparator = identical(), 
    overwrite = FALSE) {
  # Process and preparare input
  x <- attr(pairs, "x")
  y <- attr(pairs, "y")
  if (missing(on) && !missing(on)) on <- names(comparators)
  if (missing(on) || is.null(on)) stop("on is missing.")
  if (!overwrite && any(on %in% names(pairs))) 
    stop("Variable in on already present in pairs.")
  if (!all(on %in% names(x)))
    stop("Not all variables in on are present in x.")
  if (!all(on %in% names(y))) 
    stop("Not all variables in on are present in y.")
  comparators <- extend_to(on, comparators, default = default_comparator)  
  # Compare
  for (var in on) {
    cmp_fun <- comparators[[var]]
    pairs[[var]] <- cmp_fun(x[pairs$.x, ..var][[1]], y[pairs$.y, ..var][[1]])
  }
  attr(pairs, "compare_on") <- on
  attr(pairs, "comparators") <- comparators
  pairs
}


