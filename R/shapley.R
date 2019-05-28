collapse <- function(x) paste0(x, collapse = "")

#' Compute Shapley-Shorrocks Value Decompositions
#'
#'
#' @param vfun A value function.
#' @param factors A vector of factors, passed to \code{vfun}. Owen value decomposition can be
#'  can be achieved by passing a list of vectors, where then each item of the list is treated
#'  as one group. This list also can contain further lists, defining subgroups. Only two levels
#'  of nesting are currently supported.
#' @param outcomes Column names for outcome values (usually this is only one).
#' @param silent If FALSE (the default), prints a progress bar.
#' @param ... Additional arguments passed to \code{vfun}.
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
shapley <- function(vfun, factors, outcomes = "value", silent = FALSE, ...) {
    cache <- new.env(hash = TRUE, parent = emptyenv())
    get_from_cache <- function(indices) {
        if (length(indices) == 0) {
            # environments allow only character keys
            key <- "0"
        } else {
            key <- paste0(indices, collapse = "")
        }
        if (exists(key, envir = cache)) {
            get(key, envir = cache)
        } else {
            res <- vfun(unlist(factors)[indices], ...)
            if (length(res) != length(outcomes)) {
                if (!silent) close(pb)
                stop("vfun returned a different number of values than defined in outcomes")
            }
            assign(key, res, envir = cache)
            res
        }
    }

    if (is.list(factors)) {
        # Owen values
        i_factors <- unlist(factors)
        n_factors <- length(i_factors)
        # group size - this takes into account that there might be subgroups
        group_size <- sapply(1:length(factors), function(i) length(unlist(factors[[i]])))
        groups <- split(1:n_factors, rep(1:length(factors), group_size))

        # get all permutations *within* groups (give as vector)
        # TODO: this doesnt work if these are large - have to use different approach here
        group_perms <- lapply(groups, function(g) arrangements::permutations(v = g, length(g)))
        # then only the groups "play" against each other
        perms <- arrangements::permutations(v = 1:length(groups), length(groups))
        expand_grid_df <- function(...) Reduce(function(...) merge(..., by = NULL), ...)

        # the perms matrix indicates group indices, adjust and then expand the two matrices
        perms <- lapply(1:nrow(perms), function(i_row) {
            row <- perms[i_row, ]
            l <- vector(mode = "list", length = length(group_perms))
            for (i in 1:length(row)) {
                l[[i]] <- group_perms[[row[i]]]
            }
            m <- as.matrix(expand_grid_df(l))
            dimnames(m) <- NULL
            m
        })
        perms <- do.call(rbind, perms)

        # now take care of subgroups
        # not the most elegant way to do it, but it works
        subgroups <- factors[sapply(1:length(factors), function(i) is.list(factors[[i]]))]
        subgroups <- unlist(subgroups, recursive = FALSE)
        more_subgroups <- sapply(1:length(subgroups), function(i) is.list(subgroups[[i]]))
        if (sum(more_subgroups) > 0)
            stop("nesting of more than two levels not supported")
        subgroups <- lapply(subgroups, function(sg) which(i_factors %in% sg))
        for (sg in subgroups) {
            if (length(sg) == 1) next
            # find all possible permutations, and use a regex to find all instances
            group_perms <- arrangements::permutations(sg, length(sg))
            regex <- paste0(apply(group_perms, 1, collapse), collapse = "|")

            perms <- perms[grepl(regex, apply(perms, 1, collapse)), ]
        }
    } else {
        # normal Shapley decomposition
        n_factors <- length(factors)
        perms <- arrangements::permutations(n_factors, n_factors)
    }

    means <- list()

    if (!silent) message(paste("N ranks:", nrow(perms)))
    if (!silent) pb <- utils::txtProgressBar(min = 0, max = n_factors, style = 3)

    for (factor in 1:n_factors) {
        preceding <- apply(perms, 1, function(row) {
            ix <- which(row == factor)
            if (ix == 1) {
                factor
            } else {
                row[1:ix]
            }
        })
        # sort here, so it's not required further down
        # sorting ensures that cases like 01 and 10 are not computed twice
        preceding <- lapply(preceding, sort.int)

        values <- sapply(preceding, function(fs) {
            get_from_cache(fs) - get_from_cache(fs[fs != factor])
        })
        if (is.matrix(values)) {
            # in case of >1 return values of vfun
            means[[factor]] <- apply(values, 1, mean)
        } else {
            # in case of 1 return value of vfun
            means[[factor]] <- mean(values)
        }
        if (!silent) utils::setTxtProgressBar(pb, factor)
    }
    if (!silent) close(pb)

    if (is.list(factors)) {
        group_indices <- as.numeric(rep(names(groups), lengths(groups)))
        if (length(subgroups) == 0) {
            df <- data.frame(group = group_indices,
                factor = unlist(factors), stringsAsFactors = FALSE)
        } else {
            # not the most elegant way to do it, but it works
            subgroup_indices <- group_indices * 10
            i_group <- 0
            ix <- 1
            for (group in factors) {
                i_group <- i_group + 1
                if (is.list(group)) {
                    i_subgroup <- 0
                    for (sg in group) {
                        i_subgroup <- i_subgroup + 1
                        subgroup_indices[ix:(ix - 1 + length(sg))] <-
                            group_indices[ix] * 10 + i_subgroup
                        ix <- ix + length(sg)
                    }
                } else {
                    ix <- ix + length(group)
                }
            }

            df <- data.frame(group = group_indices,
                subgroup = subgroup_indices,
                factor = unlist(factors), stringsAsFactors = FALSE)
        }
    } else {
        df <- data.frame(factor = factors, stringsAsFactors = FALSE)
    }

    # add values
    m <- matrix(unlist(means), nrow = n_factors, byrow = TRUE)
    for (var in 1:length(outcomes)) {
        df[, outcomes[var]] <- m[, var]
    }

    df
}


#' EXPERIMENTAL: Compute Sampled Shapley-Shorrocks Value Decompositions
#'
#'
#' @param vfun A value function.
#' @param factors A vector of factors, passed to \code{vfun}. List for Owen values is allowed,
#'    but only one level.'
#' @param last_n An integer that specifies on how many values the standard deviation of the last
#'   samples is calculated.
#' @param precision The stopping criterion.
#' @param max_iter Maximum number of samples to be drawn for each factor. Should be reasonably large
#'   and usually doesn't matter. To adjust computation speed,
#'   it is preferred to adjust \code{precision}.
#' @param silent If FALSE (the default), prints a progress bar.
#' @param ... Additional arguments passed to \code{vfun}.
#' @return Returns a data frame with N rows, where N is the number of factors.
#' @references
#' Shapley, L. S. (1953). A value for n-person games.
#'      Contributions to the Theory of Games, 2(28), 307-317.
#'
#' Shorrocks, A. F. (2013).
#'      Decomposition procedures for distributional analysis:
#'      a unified framework based on the Shapley value. Journal of Economic Inequality, 1-28.
#' @export
shapley_sampled <- function(vfun, factors,
                            last_n = 100, precision = 1e-4, max_iter = 1e6, silent = FALSE, ...) {
    cache <- new.env(hash = TRUE, parent = emptyenv())
    get_from_cache <- function(indices) {
        if (length(indices) == 0) {
            # environments allow only character keys
            key <- "0"
        } else {
            key <- paste0(indices, collapse = "")
        }
        if (exists(key, envir = cache)) {
            get(key, envir = cache)
        } else {
            res <- vfun(unlist(factors)[indices], ...)
            if (length(res) != 1) {
                if (!silent) close(pb)
                stop("vfun returned a != one value")
            }
            assign(key, res, envir = cache)
            res
        }
    }

    n_factors <- length(unlist(factors))
    if (is.list(factors)) {
        group_size <- sapply(1:length(factors), function(i) length(unlist(factors[[i]])))
        groups <- split(1:n_factors, rep(1:length(factors), group_size))
    }

    if (last_n < 10) stop("last_n needs to be at least 10")

    contrib <- list()
    means <- list()
    for (factor in 1:n_factors) {
        contrib[[factor]] <- vector(mode = "numeric")
        means[[factor]] <- vector(mode = "numeric")
    }

    if (!silent) pb <- utils::txtProgressBar(min = 0, max = n_factors, style = 3)

    for (factor in 1:n_factors) {
        if (!silent) utils::setTxtProgressBar(pb, factor)

        for (n in 1:max_iter) {
            if (!silent & n %% 100 == 0) cat(".")
            if (is.list(factors)) {
                group_order <- sample(1:length(groups))
                seq <- unlist(lapply(group_order, function(gix) sample(groups[[gix]])))
            } else {
                seq <- sample(1:n_factors)
            }

            ix <- which(seq == factor)
            if (ix == 1) {
                preceding <- c()
            } else {
                preceding <- seq[1:(ix - 1)]
            }
            contrib[[factor]][n] <- get_from_cache(c(factor, preceding)) - get_from_cache(preceding)

            means[[factor]][n] <- mean(contrib[[factor]])

            if (n > last_n) {
                last_nvalues <- utils::tail(means[[factor]], last_n)
                if (stats::sd(last_nvalues) < precision) {
                    break
                }
            }
        }
    }
    if (!silent) close(pb)

    if (is.list(factors)) {
        group_indices <- as.numeric(rep(names(groups), lengths(groups)))
        df <- data.frame(group_indices = group_indices,
            factor = unlist(factors), stringsAsFactors = FALSE)
    } else {
        df <- data.frame(factor = unlist(factors), stringsAsFactors = FALSE)
    }
    df$value <- sapply(contrib, mean)
    df$iterations <- sapply(contrib, length)
    df$means <- I(means)

    total_diff <- get_from_cache(1:n_factors) - get_from_cache(c())
    ratio <- abs(log(total_diff / sum(df$value)))
    if (ratio > log(1.01))
        pct <- (exp(ratio) - 1) * 100
        warning(paste0("Sum of values differs ~", round(pct),
            "% from value function"))

    df
}
