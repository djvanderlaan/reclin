
#' @rdname select_n_to_m
#' @export
select_greedy <- function(pairs, threshold = NULL, weight, var = "select", 
    preselect = NULL, id_x = NULL, id_y = NULL, ...) {
  if (!methods::is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("select_greedy")
}

#' @export
select_greedy.data.frame <- function(pairs, threshold = NULL, weight = NULL, var = "select",
    preselect = NULL, id_x = NULL, id_y = NULL, ...) {
  prep <- select_preprocess(pairs, threshold, weight, preselect, id_x, id_y)
  select <- prep$select

  sel <- greedy(prep$x, prep$y, prep$w)
  select[select] <- sel

  pairs[[var]] <- select
  attr(pairs, "selection") <- var
  pairs
}

#' @export
select_greedy.ldat <- function(pairs, threshold = NULL, weight = NULL, var = "select", 
    preselect = NULL, id_x = NULL, id_y = NULL, ...) {
  prep <- select_preprocess(pairs, threshold, weight, preselect, id_x, id_y)
  select <- prep$select

  sel <- greedy(prep$x, prep$y, prep$w)
  select[select] <- sel

  pairs[[var]] <- select
  attr(pairs, "selection") <- var
  pairs
}


select_preprocess <- function(pairs, threshold = NULL, weight, preselect = NULL, 
    n = 1, m = 1, id_x = NULL, id_y = NULL) {
  # Process weight
  if (missing(weight) || is.null(weight)) weight <- attr(pairs, "score")
  if (is.null(weight)) stop("Missing weight")
  if (is.character(weight)) weight <- pairs[[weight]]
  # Proces selection: threshold/preselect
  if (!is.null(preselect)) {
    select <- if (is.character(preselect)) pairs[[preselect]] else preselect
    if (is.null(select)) stop("'", preselect, "' not found in pairs.")
    if (is_lvec(select)) select <- clone(select)
  } else {
    select <- if (is_ldat(pairs)) !lvec(nrow(pairs), "logical") else !logical(nrow(pairs))
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
  list(
    x = as_rvec(x[select]),
    y = as_rvec(y[select]),
    w = as_rvec(weight[select]),
    select = select
  )
}

