library("shapley")
context("test_owen")

test_that("rego example", {
    # built on: http://repec.org/usug2012/UK12_sunder.pdf

    ex <- function(factors) {
        # factor is 1, 2, or 3
        if (length(factors) == 3) return(.72)
        if (length(factors) == 2 & 1 %in% factors & 2 %in% factors) return(.48)
        if (length(factors) == 2 & 1 %in% factors & 3 %in% factors) return(.54)
        if (length(factors) == 2 & 2 %in% factors & 3 %in% factors) return(.30)
        if (length(factors) == 1 & 1 %in% factors) return(.24)
        if (length(factors) == 1 & 2 %in% factors) return(.12)
        if (length(factors) == 1 & 3 %in% factors) return(.18)
        if (length(factors) == 0) return(0)
    }

    # without grouping
    res <- shapley(ex, 1:3, silent = TRUE)
    expect_equal(res$value, c(.34, .16, .22))

    # with grouping
    res <- shapley(ex, list(1, c(2, 3)), silent = TRUE)
    expect_equal(res$group, c(1, 2, 2))
    expect_equal(res$value, c(.33, .165, .225))

    # calculate group values manually
    ex_union <- function(factors) {
        # factor is either "1" or "23", i.e. grouping of {1} and {2, 3}
        if (length(factors) == 2) return(ex(1:3))
        if (length(factors) == 1 & "1" %in% factors) return(ex(1))
        if (length(factors) == 1 & "23" %in% factors) return(ex(c(2, 3)))
        if (length(factors) == 0) return(0)
    }
    res <- shapley(ex_union, c("1", "23"), silent = TRUE)
    expect_equal(res$value, c(.33, .165 + .225))
})

test_that("regression example", {
    reg <- function(regressors) {
        if (length(regressors) == 0) return(0)
        formula <- paste0("mpg ~ ", paste(regressors, collapse = "+"))
        m <- summary(lm(formula, data = mtcars))
        m[["r.squared"]]
    }

    # all groups only one factor
    expect_equal(
        shapley(reg, c("cyl", "disp", "hp"), silent = TRUE)$value,
        shapley(reg, list("cyl", "disp", "hp"), silent = TRUE)$value)

    # three groups
    ungrouped <- shapley(reg, c("cyl", "disp", "hp", "drat", "wt", "qsec"),
        silent = TRUE)
    with_groups <- shapley(reg, list(c("cyl"), c("disp", "hp", "drat"), c("wt", "qsec")),
        silent = TRUE)
    supergroups <- shapley(reg, c("cyl", "disp+hp+drat", "wt+qsec"),
        silent = TRUE)
    expect_equal(sum(ungrouped$value), sum(with_groups$value))
    expect_equal(sum(ungrouped$value), sum(supergroups$value))
    expect_equal(sum(with_groups$value[1]), supergroups$value[[1]])
    expect_equal(sum(with_groups$value[2:4]), supergroups$value[[2]])
    expect_equal(sum(with_groups$value[5:6]), supergroups$value[[3]])
})
