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
        res <- shapley_sampled(reg,
                c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                last_n = 50,
                precision = .1,
                silent = TRUE)
    })

    expect_equal(nrow(res), 10)
    expect_equal(nrow(res), 10)
})
