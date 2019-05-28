library("shapley")
context("test_shapley_sampled")

reg <- function(regressors) {
    if (length(regressors) == 0) return(0)
    formula <- paste0("mpg ~ ", paste(regressors, collapse = "+"))
    m <- summary(lm(formula, data = mtcars))
    m[["r.squared"]]
}

test_that("regression example", {
    expect_warning({
        res1 <- shapley_sampled(reg,
                c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                last_n = 10,
                precision = .01,
                silent = TRUE)
    })

    expect_equal(nrow(res1), 10)
})

test_that("regression example", {
    res2 <- shapley(reg,
              list(c("cyl", "disp", "hp", "drat"), c("qsec", "vs", "am", "gear")),
              silent = TRUE)
    expect_warning({
        res3 <- shapley_sampled(reg,
                  list(c("cyl", "disp", "hp", "drat"), c("qsec", "vs", "am", "gear")),
                  precision = .05,
                  silent = TRUE)
    })
    expect_lt(mean(exp(abs(log(res2$value / res3$value)))), 1.20)
})
