#' Compute Shapley-Shorrocks Value Decompositions
#'
#'
#' @param vfun A value function.
#' @param factors A vector of factors, passed to \code{vfun}.
#' @param outcomes Column names for outcome values (usually this is only one).
#' @param silent If FALSE (the default), prints a progress bar.
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
shapley <- function(vfun, factors, outcomes = "value", silent = FALSE) {
    cache <- new.env(hash = TRUE, parent = emptyenv())
    get_vfun <- function(indices) {
        if (length(indices) == 0) {
            key <- "0"
        } else {
            key <- paste0(indices, collapse = "")
        }
        if (exists(key, envir = cache)) {
            get(key, envir = cache)
        } else {
            res <- vfun(factors[indices])
            if (length(res) != length(outcomes)) {
                if (!silent) close(pb)
                stop("vfun returned a different number of values than defined in outcomes")
            }
            assign(key, res, envir = cache)
            res
        }
    }

    n_factors <- length(factors)
    P <- arrangements::permutations(n_factors, n_factors)
    means <- list()

    if (!silent) pb <- utils::txtProgressBar(min = 0, max = n_factors, style = 3)
    for (factor in 1:n_factors) {
        preceding <- apply(P, 1, function(row) {
            ix <- which(row == factor)
            if (ix == 1) {
                factor
            } else {
                row[1:ix]
            }
        })
        preceding <- preceding[lengths(preceding) != 0]
        preceding <- lapply(preceding, sort.int)

        values <- sapply(preceding, function(fs) {
            get_vfun(fs) - get_vfun(fs[fs != factor])
        })
        if (is.matrix(values)) {
            means[[factor]] <- apply(values, 1, mean)
        } else {
            means[[factor]] <- mean(values)
        }
        if (!silent) utils::setTxtProgressBar(pb, factor)
    }
    if (!silent) close(pb)

    m <- matrix(unlist(means), nrow = n_factors, byrow = TRUE)
    df <- data.frame(factor = factors)
    for (var in 1:length(outcomes)) {
        df[, outcomes[var]] <- m[, var]
    }
    df
}
