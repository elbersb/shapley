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
