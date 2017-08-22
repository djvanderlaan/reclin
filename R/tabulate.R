


#' @import ldat
#' @import lvec
#' @export
tabulate <- function(pairs, comparators, by) {
  if (missing(comparators)) comparators <- attr(pairs, "comparators")
  if (missing(by)) 
    by <- if (missing(comparators)) attr(pairs, "by") else names(comparators)
  
  chunks <- chunk(pairs)
  tab <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    d <- slice_range(pairs, range = chunks[[i]], as_r = TRUE)
    for (col in by) d[[col]] <- comparators[[col]](d[[col]])
    tab[[i]] <- d %>% group_by_(.dots = by) %>% summarise(n = n())
  }
  bind_rows(tab) %>% group_by_(.dots = by) %>% summarise(n = sum(n)) %>%
    ungroup() %>% as.data.frame()
}
