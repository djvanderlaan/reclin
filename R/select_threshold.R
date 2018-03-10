

select_threshold <- function(pairs, weight, threshold, var = "select") {
  if (!is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("select_threshold")
}

select_threshold.ldat <- function(pairs, weight, threshold, var = "select") {  
  if (is.character(weight)) weight <- pairs[[weight]]
  pairs[[var]] <- weight > threshold
  attr(pairs, "selection") <- var
  pairs
}

select_threshold.data.frame <- function(pairs, weight, threshold, 
    var = "select") {  
  if (is.character(weight)) weight <- pairs[[weight]]
  pairs[[var]] <- weight > threshold
  attr(pairs, "selection") <- var
  pairs
}

