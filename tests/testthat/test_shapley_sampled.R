library("shapley")
context("test_shapley_sampled")

reg <- function(regressors) {
    if (length(regressors) == 0) return(0)
    formula <- paste0("mpg ~ ", paste(regressors, collapse = "+"))
    m <- summary(lm(formula, data = mtcars))
    m[["r.squared"]]
}

test_that("regression example", {
    res1 <- shapley_sampled(reg,
            c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
            last_n = 10,
            precision = .01,
            silent = TRUE)

    expect_equal(nrow(res1), 10)
})

test_that("regression example", {
    res2 <- shapley(reg,
              list(c("cyl", "disp", "hp", "drat"), c("qsec", "vs", "am", "gear")),
              silent = TRUE)
    res3 <- shapley_sampled(reg,
              list(c("cyl", "disp", "hp", "drat"), c("qsec", "vs", "am", "gear")),
              precision = .01,
              silent = TRUE)

    expect_lt(mean(exp(abs(log(res2$value / res3$value)))), 1.20)
})


reg2 <- function(regressors) {
    if (length(regressors) == 0) return(c(0, 0))
    formula <- paste0("mpg ~ ", paste(regressors, collapse = "+"))
    m <- summary(lm(formula, data = mtcars))
    # simply return the same value twice
    c(m[["r.squared"]], m[["r.squared"]])
}

test_that("more than one outcome", {
    expect_error(
        shapley_sampled(reg2,
              c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
              last_n = 10,
              precision = .01,
              silent = TRUE), "returned a different number of value")

    res1 <- shapley_sampled(reg,
            c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
            last_n = 10,
            precision = .001,
            silent = TRUE)

    res2 <- shapley_sampled(reg2,
        c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
        outcomes = c("r2", "r2.2"),
        last_n = 10,
        precision = .001,
        silent = TRUE)

    expect_equal("r2" %in% names(res2), TRUE)
    expect_equal("r2.2" %in% names(res2), TRUE)
    expect_equal(!"value" %in% names(res2), TRUE)
    expect_equal(nrow(res1), 10)
    expect_gt(cor(res1$value, res2$r2), .5)
})
