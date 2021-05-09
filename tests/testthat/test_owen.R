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
    res <- owen(ex, list(1, c(2, 3)), silent = TRUE)
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

reg <- function(regressors) {
    if (length(regressors) == 0) return(0)
    formula <- paste0("mpg ~ ", paste(regressors, collapse = "+"))
    m <- summary(lm(formula, data = mtcars))
    m[["r.squared"]]
}

test_that("regression example", {
    # all groups only one factor
    expect_equal(
        shapley(reg, c("cyl", "disp", "hp"), silent = TRUE)$value,
        owen(reg, list("cyl", "disp", "hp"), silent = TRUE)$value)

    owen(reg, list("cyl", list("disp"), "hp"), silent = TRUE)

    # three groups
    ungrouped <- shapley(reg, c("cyl", "disp", "hp", "drat", "wt", "qsec"),
        silent = TRUE)
    with_groups <- owen(reg, list(c("cyl"), c("disp", "hp", "drat"), c("wt", "qsec")),
        silent = TRUE)
    supergroups <- shapley(reg, c("cyl", "disp+hp+drat", "wt+qsec"),
        silent = TRUE)
    expect_equal(sum(ungrouped$value), sum(with_groups$value))
    expect_equal(sum(ungrouped$value), sum(supergroups$value))
    expect_equal(sum(with_groups$value[1]), supergroups$value[[1]])
    expect_equal(sum(with_groups$value[2:4]), supergroups$value[[2]])
    expect_equal(sum(with_groups$value[5:6]), supergroups$value[[3]])
})

test_that("Owen with two levels", {
    expect_equal(
        shapley(reg, c("cyl", "disp", "hp"), silent = TRUE)$value,
        owen(reg, list("cyl", list("disp"), "hp"), silent = TRUE)$value)

    supergroups <- shapley(reg, c("cyl", "disp+hp+drat", "gear+wt+qsec"),
        silent = TRUE)
    two_levels <- owen(reg,
            list("cyl",
                 list("disp", c("hp", "drat")),
                 list("gear", c("wt", "qsec"))),
        silent = TRUE)
    expect_equal(sum(supergroups$value), sum(two_levels$value))
    expect_equal(supergroups$value[1], two_levels$value[[1]])
    expect_equal(supergroups$value[2], sum(two_levels$value[2:4]))
    expect_equal(supergroups$value[3], sum(two_levels$value[5:7]))

    supergroups2 <- owen(reg, list("cyl", list("disp", "hp+drat"), list("gear", "wt+qsec")),
        silent = TRUE)
    expect_equal(supergroups2$value[1], two_levels$value[[1]])
    expect_equal(supergroups2$value[2], two_levels$value[[2]])
    expect_equal(supergroups2$value[3], sum(two_levels$value[3:4]))
    expect_equal(supergroups2$value[4], two_levels$value[[5]])
    expect_equal(supergroups2$value[5], sum(two_levels$value[6:7]))
})

test_that("Three levels", {
    # this is ok
    owen(function(x) 1, list(
        list(c(1, 2, 3), c(4)),
        c(5, 6)), silent = TRUE)

    # this is not ok (grouping 2, 3 in their own sub-sub-group)
    expect_error(owen(function(x) 1, list(
        list(list(1, list(2, 3)), c(4)),
        list(5, 6)), silent = TRUE))
})

test_that("Owen 1977 examples", {
    three <- function(f) {
        if (length(f) == 0) return(0)
        if (length(f) == 1) return(0)
        if (length(f) == 2 & 2 %in% f & 3 %in% f) return(0)
        if (length(f) == 2 & 1 %in% f & 2 %in% f) return(80)
        if (length(f) == 2 & 1 %in% f & 3 %in% f) return(100)
        if (length(f) == 3) return(100)
    }
    expect_equal(round(shapley(three, 1:3, silent = TRUE)$value), c(63, 13, 23))

    expect_equal(owen(three, list(c(1, 2), 3), silent = TRUE)$value, c(70, 20, 10))
    expect_equal(owen(three, list(c(1, 3), 2), silent = TRUE)$value, c(70, 30, 0))
    expect_equal(owen(three, list(1, c(2, 3)), silent = TRUE)$value, c(50, 20, 30))
})


