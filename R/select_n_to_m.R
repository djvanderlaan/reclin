
select_n_to_m <- function(pairs, weight, var = "select", threshold = NULL, 
    preselect = NULL, n = 1, m = 1, id_x = NULL, id_y = NULL, ...) {
  UseMethod("select_n_to_m")
}

select_n_to_m.data.frame <- function(pairs, weight, var = "select", threshold = NULL, 
    preselect = NULL, n = 1, m = 1, id_x = NULL, id_y = NULL, ...) {
  
}

select_n_to_m.ldat <- function(pairs, weight, var = "select", threshold = NULL, 
    preselect = NULL, n = 1, m = 1, id_x = NULL, id_y = NULL, ...) {
  # Process weight
  if (is.character(weight)) weight <- pairs[[weight]]
  # Proces selection: threshold/preselect
  if (!is.null(preselect)) {
    select <- if (is.character(preselect)) {
      clone(pairs[[preselect]])
    } else {
      clone(preselect)
    }
    if (is.null(select)) stop("'", preselect, "' not found in pairs.")
  } else {
    select <- !lvec(nrow(pairs), "logical") 
  }
  if (!is.null(threshold)) {
    select <- select & (weight > threshold)
  }
  # When id_x and id_y are not given it is assumed that every row in x and y are
  # unique elements; when given look voor object identifier in resp x and y
  if (!is.null(id_x) && !missing(id_x)) {
    if (is.character(id_x)) {
      x <- attr(pairs, "x")[[id_x]]
    } else {
      if (length(id_x) != length(x)) 
        stop("Length of id_x doesn't match length of x.")
      x <- id_x
    }
  } else x <- pairs$x
  if (!is.null(id_y) && !missing(id_y)) {
    if (is.character(id_y)) {
      y <- attr(pairs, "y")[[id_y]]
    } else {
      if (length(id_y) != length(y)) 
        stop("Length of id_y doesn't match length of y.")
      y <- id_y
    }
  } else y <- pairs$y
  # Select possible matches
  x <- as_rvec(x[select])
  y <- as_rvec(y[select])
  sel_ind <-  match_n_to_m(x, y, as_rvec(weight[select]), n = n, m = m)
  sel <- logical(length(x))
  sel[sel_ind] <- TRUE
  select[select] <- sel
  pairs[[var]] <- select
  attr(pairs, "selection") <- var
  pairs
}
