#' Calculate weights and probabilities for pairs
#' 
#' @param object an object of type `simple_em` as produced by 
#'   \code{\link{fs_em}}.
#' @param pairs a object with pairs for which to calculate weights.
#' @param newdata an alternative name for the `pairs` argument. Specify 
#'   `newdata` or `pairs`. 
#' @param type a character vector of length one speicifying what to calculate. 
#'   See results for more information.
#'   
#' @return 
#' In case of `type == "weights"` returns a vector (\code{\link{lvec}} or
#' regular R-vector depending on the type of `pairs`). with the linkage weights. 
#' In case of `type == "mpost"` returns a vector with the posterior mp-probabilities
#' (probability that a pairs is a match). In case of `type == "probs"` returns a
#' data.frame or \code{\link{ldat}} with the m- and u-probabilities and postertior
#' m- and u probabilities. In case `type == "all"` returns a data.frame or 
#' \code{\link{ldat}} with both probabilities and weights. 
#' 
#' @import lvec.stats
#' @import lvec
#' @export
predict.simple_em <- function(object, pairs = newdata, newdata = NULL, 
    type = c("weights", "mpost", "probs", "all"), ...) {
  # Process input
  type <- match.arg(type)
  if (is.null(pairs)) pairs <- newdata
  if (is.null(pairs)) stop("Missing pairs or newdata.")
  by <- names(object$mprobs)
  # Initialise end result and for-loop
  weights <- if (is_ldat(pairs)) lvec(nrow(pairs), type="numeric") else 
    numeric(nrow(pairs))
  mprobs  <- if (is_ldat(pairs)) lvec(nrow(pairs), type="numeric") else 
    numeric(nrow(pairs))
  uprobs  <- if (is_ldat(pairs)) lvec(nrow(pairs), type="numeric") else 
    numeric(nrow(pairs))
  chunks   <- chunk(pairs)
  # Process data in chunks
  for (c in chunks) {
    
    d <- slice_range(pairs, range = c, as_r = TRUE) 
    
    wc <- rep(0, nrow(d))
    mc <- rep(1, nrow(d))
    uc <- rep(1, nrow(d))
    
    for (col in by) {
      pm <- (1 - object$mprobs[[col]]) +
                (2 * object$mprobs[[col]] - 1) * d[[col]]
      
      pu <- (1 - object$uprobs[[col]]) + 
                (2 * object$uprobs[[col]] - 1) * d[[col]]
      w  <- log(pm / pu)
      
      # Give pairs with missing values a weight 0 for correspongin variable
      w[is.na(w)] <- 0
      # Add weight, mprob, uprob to total vectors
      wc <- wc + w
      mc <- mc * pm
      uc <- uc * pu
    }
    
    weights <- lset(weights, range = c, values = wc)
    mprobs  <- lset(mprobs,  range = c, values = mc)
    uprobs  <- lset(uprobs,  range = c, values = uc)
  }
  if (type == "weights") {
   weights 
  } else if (type == "mpost") {
    mprobs * object$p / (mprobs * object$p + uprobs * (1 - object$p))
  } else {
    mpost <- mprobs * object$p / (mprobs * object$p + uprobs * (1 - object$p))
    res <- if (is_ldat(pairs))  ldat(mprob = mprobs, uprob = uprobs, 
        mpost = mpost, upost = 1 - mpost) else 
      data.frame(mprob = mprobs, uprob = uprobs, mpost = mpost, 
        upost = 1 - mpost)
    if (type == "all") res$weights <- weights
    res
  } 
}

# 
# 
#     
#     
#     for (j in seq_along(link$by_x)) {
#       col   <- link$by_x[j]
#       # retrieve match status for current column
#       m     <- link$pairs[[col]][c]
#       # retrieve data for current column
#       x     <- link$x[link$pairs$x[c], col]
#       y     <- link$y[link$pairs$y[c], link$by_y[j]]
#       # == calculate m-probabilities
#       mprob <- numeric(length(m))
#       # set for missing values m = 0.5, this will result in weights of 0
#       m[is.na(x) | is.na(y)] <- 0.5
#       # we can have different m-probabilities for different groups; keep track
#       # for which groups the m-probabilities are already calculated.
#       remain <- rep(TRUE, length(m))
#       if (length(mprobs[[col]]) > 1) {
#         split <- attr(mprobs[[col]], "split")
#         for (s in seq_along(split)) {
#           sel <- x %in% split[[s]]
#           sel <- sel & remain
#           mprob[sel]  <- mprobs[[col]][s]
#           remain[sel] <- FALSE
#         }
#       }
#       mprob[remain] <- mprobs[[col]][length(mprobs[[col]])]
#       # == calculate u-probabilities
#       uprob  <- numeric(length(m))
#       # we can have different u-probabilities for different groups; keep track
#       # for which groups the u-probabilities are already calculated.
#       remain <- rep(TRUE, length(m))
#       if (length(uprobs[[col]]) > 1) {
#         split <- attr(uprobs[[col]], "split")
#         for (s in seq_along(split)) {
#           sel <- x %in% split[[s]]
#           sel <- sel & remain
#           uprob[sel]  <- uprobs[[col]][s]
#           remain[sel] <- FALSE
#         }
#       }
#       uprob[remain] <- uprobs[[col]][length(uprobs[[col]])]
#       # == update weights
#       d     <- (1-mprob) + (2*mprob - 1)*m
#       n     <- (1-uprob) + (2*uprob - 1)*m
#       wc    <- log(d/n)
#       wc[is.na(wc)] <- 0
#       weights[c] <- weights[c] + wc
#       if (muprobs) {
#         mp[c] <- mp[c] * d
#         up[c] <- up[c] * n
#       }
#     }
#     # update progress bar
#     if (progress) {
#       setTxtProgressBar(pb, i)
#       i <- i + 1
#     }
#   }
#   # close progress bar
#   if (progress) close(pb)
#   # generate output
#   if (!add) {
#     if (muprobs) {
#       return(list(weights=weights, mprobs=mp, uprobs=up))
#     } else { 
#       return(weights)
#     }
#   } else {
#     link$weights <- weights
#     if (muprobs) {
#       link$mprobs <- mp
#       link$uprobs <- up
#     }
#     return(link)
#   }
# }