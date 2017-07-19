

#' @import lvec
#' @import lvec.stats
#' @import dplyr
#' @export
simple_blocking <- function(x, y, blocking_var = NULL, large = TRUE, 
    add_xy = TRUE, chunk_size = 1E7) {
  if (missing(chunk_size)) chunk_size <- getOption("chunk_size", chunk_size)
  # when large = TRUE: chunk x; generate pairs for each chunk and append those
  # to the pairs data.frame
  if (large) {
    nblocks <- if (is.null(blocking_var)) 1 else nrow(unique(x[blocking_var]))
    chunks <- chunk(x, chunk_size = round(nblocks * 1E7/nrow(y)))
    pairs <- NULL
    for (i in seq_along(chunks)) {
      p <- simple_blocking(slice_range(x, range = chunks[[i]], as_r = TRUE), 
        y, blocking_var, large = FALSE)
      p$x <- p$x + chunks[[i]][1] - 1
      if (is.null(pairs)) {
        pairs <- as_ldat(p)
      } else {
        pairs <- lvec.stats::append(pairs, p, clone = FALSE)
      }  
    }
  } else {
    if (missing(blocking_var) || is.null(blocking_var)) {
      pairs <- expand.grid(x = seq_len(nrow(x)), y = seq_len(nrow(y)))
    } else {
      a <- x %>% select_(.dots = blocking_var) %>% mutate(x = row_number())
      b <- y %>% select_(.dots = blocking_var) %>% mutate(y = row_number())
      pairs <- inner_join(a, b, by = blocking_var) %>% select(x, y)
    }
  }
  # Add attributes to pairs
  pairs <- structure(pairs, blocking_var = blocking_var)
  class(pairs) <- c("simple_blocking", "pairs", class(pairs))
  if (add_xy) {
    attr(pairs, "x") <- x
    attr(pairs, "y") <- y
  }
  pairs
}
