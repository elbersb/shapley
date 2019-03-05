#' Compute Shapley-Shorrocks Value Decompositions
#'
#'
#' @param vfun A value function.
#' @param factors A vector of factors, passed to \code{vfun}.
#' @param silent If FALSE (the default), prints a progress bar for each factor.
#' @return Returns a data frame with N rows, where N is the number of factors.
#' @references
#' Shapley, L. S. (1953). A value for n-person games.
#'      Contributions to the Theory of Games, 2(28), 307-317.
#'
#' Shorrocks, A. F. (2013).
#'      Decomposition procedures for distributional analysis:
#'      a unified framework based on the Shapley value. Journal of Economic Inequality, 1-28.
#' @import arrangements
#' @export
shapley <- function(vfun, factors, silent = FALSE) {
    cache <- new.env(hash = TRUE, parent = emptyenv())
    get_vfun <- function(indices) {
        if (length(indices) == 0) {
            key <- "NONE"
        } else {
            key <- paste0(sort(indices), collapse = "")
        }
        if (exists(key, envir = cache)) {
            get(key, envir = cache)
        } else {
            res <- vfun(factors[indices])
            assign(key, res, envir = cache)
            res
        }
    }

    n_factors <- length(factors)
    P <- arrangements::permutations(n_factors, n_factors)
    means <- list()

    for (factor in 1:n_factors) {
        if (!silent) pb <- utils::txtProgressBar(min = 0, max = nrow(P), style = 3)
        values <- c()

        for (ordering in 1:nrow(P)) {
            if (!silent) utils::setTxtProgressBar(pb, ordering)

            ix <- which(P[ordering, ] == factor)
            if (ix == 1) {
                preceding <- c()
            } else {
                preceding <- P[ordering, 1:(ix - 1)]
            }
            values <- c(values, get_vfun(c(factor, preceding)) - get_vfun(preceding))
        }
        if (!silent) close(pb)

        means[[factor]] <- mean(values)
    }

    data.frame(factor = factors, value = unlist(means))
}
