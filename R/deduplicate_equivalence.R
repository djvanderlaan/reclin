

#' Deduplicatin using equivalence groups
#' 
#' @export
deduplicate_equivalence <- function(pairs, var = "duplicate_groups", selection, x) {
  if (!methods::is(pairs, "pairs")) stop("pairs should be an object of type 'pairs'.")
  UseMethod("deduplicate_equivalence")
}


#' @export
deduplicate_equivalence.data.frame <- function(pairs, var = "duplicate_groups", 
    selection, x) {
  if (missing(selection)) selection <- NULL
  if (missing(x)) x <- NULL
  deduplicate_equivalence_impl(pairs, var, selection, x)
}

#' @export
deduplicate_equivalence.ldat <- function(pairs, var = "duplicate_groups", 
    selection, x) {
  if (missing(selection)) selection <- NULL
  if (missing(x)) x <- NULL
  deduplicate_equivalence_impl(pairs, var, selection, x)
}

deduplicate_equivalence_impl <- function(pairs, var, selection, x) {
  # Process x
  if (missing(x) || is.null(x)) x <- attr(pairs, "x")
  if (is.null(x)) stop("Missing x")
  # Process selection
  if (missing(selection) || is.null(selection)) 
    selection <- attr(pairs, "selection")
  if (is.null(selection)) stop("Missing selection")
  if (is.character(selection)) selection <- pairs[[selection]]
  # 
  tmp <- as.data.frame(pairs[selection, c("x", "y")])
  x[[var]] <- equivalence(seq_len(nrow(x)), tmp)
  x
}