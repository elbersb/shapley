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
        m <- lm(formula, data = mtcars)
        c(summary(m)$r.squared, summary(m)$adj.r.squared)
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
