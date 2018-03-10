

select_greedy <- function(pairs, weight, var = "select", threshold = NULL, 
    preselect = NULL) {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("select_greedy")
}

select_greedy.ldat <- function(pairs, weight, var = "select", threshold = NULL, 
    preselect = NULL) {
  if (is.character(weight)) weight <- pairs[[weight]]
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
  x <- as_rvec(pairs$x[select])
  y <- as_rvec(pairs$y[select])
  sel <- greedy(x, y, as_rvec(weight[select]))
  select[select] <- sel
  pairs[[var]] <- select
  attr(pairs, "selection") <- var
  pairs
}

select_greedy.data.frame <- function(pairs, weight, var = "select", threshold = NULL, 
    preselect = NULL) {
  if (is.character(weight)) weight <- pairs[[weight]]
  if (!is.null(preselect)) {
    select <- if (is.character(preselect)) {
      pairs[[preselect]]
    } else {
      preselect
    }
    if (is.null(select)) stop("'", preselect, "' not found in pairs.")
  } else {
    select <- !logical(nrow(pairs)) 
  }
  if (!is.null(threshold)) {
    select <- select & (weight > threshold)
  }
  x <- as_rvec(pairs$x[select])
  y <- as_rvec(pairs$y[select])
  sel <- greedy(x, y, as_rvec(weight[select]))
  select[select] <- sel
  pairs[[var]] <- select
  attr(pairs, "selection") <- var
  pairs
}

