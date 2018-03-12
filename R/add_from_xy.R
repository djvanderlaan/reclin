

add_from_x <- function(pairs, ...) {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")  
  UseMethod("add_from_x")
}

add_from_x.data.frame <- function(pairs, ...) {
  add_from_xy_impl(pairs, "x", ...)
}

add_from_x.ldat <- function(pairs, ...) {
  add_from_xy_impl(pairs, "x", ...)
}

add_from_y <- function(pairs, ...) {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")  
  UseMethod("add_from_y")
}

add_from_y.data.frame <- function(pairs, ...) {
  add_from_xy_impl(pairs, "y", ...)
}

add_from_y.ldat <- function(pairs, ...) {
  add_from_xy_impl(pairs, "y", ...)
}

add_from_xy_impl <- function(pairs, from = c("x", "y"), ...) {
  from <- match.arg(from)
  d <- attr(pairs, from)
  variables <- list(...)
  for (i in seq_along(variables)) {
    var <- variables[[i]]
    if (!is.character(var) || length(var) != 1) 
      stop("Variable is not a character vector of length 1.")
    varname <- names(variables)[i]
    if (is.null(varname) || varname == "") varname <- var
    if (varname %in% names(pairs)) 
      stop("'", varname, "' already exists in pairs.")
    v <- if (is_ldat(pairs)) as_lvec(d[[var]]) else d[[var]]
    pairs[[varname]] <- v[pairs[[from]]]
  }
  pairs
}
