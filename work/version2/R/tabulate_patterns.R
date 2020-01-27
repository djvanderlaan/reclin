


tabulate_patterns <- function(pairs, on, comparators, complete = TRUE, ...) {
  UseMethod("tabulate_patterns", pairs)
}


tabulate_patterns.pairs <- function(pairs, on, comparators, complete = TRUE) {
  # Process arguments
  if (missing(comparators) || is.null(comparators)) 
    comparators <- attr(pairs, "comparators")
  if (missing(on) || is.null(on)) 
    on <- if (missing(comparators)) 
      attr(pairs, "compare_on") else names(comparators)
  # Tabulate
  for (var in on) 
    pairs[[var]] <- comparators[[var]](pairs[[var]])
  tab <- pairs[, .(n = .N), by = on]
  
  # Add patterns not present in dataset
  if (complete) {
    possible_patterns <- lapply(tab[, ..on], function(x) {
      u <- unique(x)
      if (is.factor(x)) union(x, levels(x)) else u
    })
    possible_patterns <- do.call(CJ, possible_patterns)
    tab <- tab[possible_patterns, , on = on]
    tab$n[is.na(tab$n)] <- 0
  }
  tab
}


tabulate_patterns.cluster_pairs <- function(pairs, on, comparators, 
    complete = TRUE) {
  # Process arguments
  if (missing(on) || is.null(on)) 
    on <- if (missing(comparators) || is.null(comparators)) 
      attr(pairs, "compare_on") else names(comparators)
  if (missing(comparators)) comparators <- NULL
  # Run tabulate_pairs on all workers
  tabs <- clusterCall(pairs$cluster, function(name, on, comparators) {
    env <- reclin_env[[name]]
    pairs <- env$pairs
    tabulate_patterns(pairs, on = on, comparators = comparators, 
      complete = FALSE)
  }, name= pairs$name, on = on, comparators = comparators)
  # Combine results
  tab <- rbindlist(tabs)
  print(on)
  tab <- tab[, .(n = sum(n)), by = on]
  # # Add patterns not present in dataset
  # if (complete) {
  #   possible_patterns <- lapply(tab[, ..on], function(x) {
  #     u <- unique(x)
  #     if (is.factor(x)) union(x, levels(x)) else u
  #   })
  #   possible_patterns <- do.call(CJ, possible_patterns)
  #   tab <- tab[possible_patterns, , on = on]
  #   tab$n[is.na(tab$n)] <- 0
  # }
  tab
}


