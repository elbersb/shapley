library("shapley")
context("test_shapley")

test_that("glove game", {
    # https://en.wikipedia.org/wiki/Shapley_value#Glove_game
    glove <- function(factors) {
        if (length(factors) > 1 & 3 %in% factors) return(1)
        return(0)
    }

    results <- shapley(glove, c(1, 2, 3))
    expect_equal(results$value, c(1 / 6, 1 / 6, 4 / 6))
})

test_that("glove game - different return name", {
    # https://en.wikipedia.org/wiki/Shapley_value#Glove_game
    glove <- function(factors) {
        if (length(factors) > 1 & 3 %in% factors) return(1)
        return(0)
    }

    results <- shapley(glove, c(1, 2, 3), outcomes = "contribution")
    expect_equal(results$contribution, c(1 / 6, 1 / 6, 4 / 6))
})

test_that("airport problem", {
    # https://en.wikipedia.org/wiki/Airport_problem
    airport <- function(factors) {
        if (length(factors) == 0) return(0)
        if ("D" %in% factors) return(18)
        if ("C" %in% factors) return(13)
        if ("B" %in% factors) return(11)
        if ("A" %in% factors) return(8)
    }

    results <- shapley(airport, c("A", "B", "C", "D"))
    expect_equal(results$value, c(2, 3, 4, 9))
})

test_that("silent", {
    simple <- function(factors = c()) {
        value <- 0
        if ("A" %in% factors) value <- value + 1
        if ("B" %in% factors) value <- value + 2
        return(value)
    }

    expect_silent(shapley(simple, c("A", "B"), silent = TRUE))
    expect_output(shapley(simple, c("A", "B"), silent = FALSE))
})

test_that("two return values", {
    reg <- function(regressors) {
        if (length(regressors) == 0) return(c(0, 0))
        formula <- paste0("mpg ~ ", paste(regressors, collapse = "+"))
        m <- summary(lm(formula, data = mtcars))
        c(m[["r.squared"]], m[["adj.r.squared"]])
    }

    expect_error(shapley(reg, c("wt", "qsec", "am"), silent = FALSE))
    expect_error(shapley(reg, c("wt", "qsec", "am"), silent = TRUE))

    # working two return values
    results <- shapley(reg, c("wt", "qsec", "am"), outcomes = c("r2", "adjr2"), silent = TRUE)
    expect_equal(nrow(results), 3)
    expect_equal(ncol(results), 3)
    expect_equal(names(results), c("factor", "r2", "adjr2"))
    expect_equal(round(results$r2, 2), c(.48, .16, .21))
    expect_equal(sum(results$r2), reg(c("wt", "qsec", "am"))[[1]])
})

test_that("additional arguments to vfun", {
    gini <- function(x) {
        # simplified from package ineq
        n <- length(x)
        x <- sort(x)
        G <- sum(x * 1L:n)
        G <- 2 * G / sum(x) - (n + 1L)
        G / n
    }

    zid <- function(factors, data) {
        cntf <- data[["MarketIncome"]]  # baseline for counterfactual income
        for (f in factors)
            cntf <- cntf + data[[f]]
        gini(cntf)
    }

    # example from:
    # Enami, A., N. Lustig, and R. Aranda (2018).
    #   Analytic Foundations: Measuring the Redistributive Impact of Taxes and Transfers.
    #   In: N. Lustig (Ed.), Commitment to Equity Handbook. Washington, D.C.: Brookings, 56-115.
    income1 <- data.frame(
        MarketIncome = c(1, 20, 30, 40, 50),
        Tax = -5,
        Transfer = c(9, 7, 5, 3, 1),
        FinalIncome = c(5, 22, 30, 38, 46))

    income2 <- data.frame(
        MarketIncome = c(1, 20, 30, 40, 50),
        Tax1 = c(0, -1, -2, -3, -4),
        Tax2 = c(-5, -4, -3, -2, -1),
        Transfer = c(9, 7, 5, 3, 1),
        FinalIncome = c(5, 22, 30, 38, 46))

    res1 <- shapley(zid, c("Tax", "Transfer"), silent = TRUE, data = income1)
    res2 <- shapley(zid, list(c("Tax1", "Tax2"), "Transfer"), silent = TRUE, data = income2)
    expect_equal(res1$value[[1]], sum(res2$value[1:2]))
    expect_equal(res1$value[[2]], sum(res2$value[[3]]))
})
