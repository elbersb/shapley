if (requireNamespace("lintr", quietly = TRUE)) {
    context("lintr")

    test_that("Package Style", {

        lintr::expect_lint_free(linters = lintr::with_defaults(
            line_length_linter = lintr::line_length_linter(100)))
    })
}
