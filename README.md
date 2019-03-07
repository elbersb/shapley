
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shapley

[![CRAN
Version](https://www.r-pkg.org/badges/version/shapley)](https://CRAN.R-project.org/package=shapley)
[![Build
Status](https://travis-ci.org/elbersb/shapley.svg?branch=master)](https://travis-ci.org/elbersb/shapley)
[![Coverage
status](https://codecov.io/gh/elbersb/shapley/branch/master/graph/badge.svg)](https://codecov.io/github/elbersb/shapley?branch=master)

The Shapley value is a concept from game theory that quantifies how much
each player contributes to the game outcome (Shapley 1953). The concept,
however, has many more use cases: it provides a method to quantify the
importance of predictors in regression analysis or machine learning
models, and can be used in a wide variety of decomposition problems
(Shorrocks 2013). Most implementations focus on one narrow use case,
although the algorithm for the Shapley value decomposition is always the
same – it is just the concrete value function that varies. This package
provides a simple algorithm for the Shapley value decomposition, and
also supports hierarchical decomposition using the Owen value.

## Installation

``` r
devtools::install_github("elbersb/shapley")
```

## Usage

The package provides a `shapley` function that takes two main arguments:
the value function and a vector of factor names. The value function
needs to be an R function that takes one or more arguments, where the
first argument defines the factors that are included in the calculation
of the outcome value. The `shapley` function will call the value
function repeatedly, each time with a different set of factors.

For a very simple example, consider that an outcome is determined by two
factors “A” and “B”, which contribute 1 and 2, respectively. (The
factors are linearly additive, which makes the use of Shapley value
decomposition unnecessary, but it works as an illustration.) The value
function is thus defined as:

``` r
simple <- function(factors = c()) {
    value <- 0
    if ("A" %in% factors) value <- value + 1
    if ("B" %in% factors) value <- value + 2
    return(value)
}
```

We now supply the value function to `shapley`, along with the factor
names:

``` r
shapley(simple, c("A", "B"), silent = TRUE)
#>   factor value
#> 1      A     1
#> 2      B     2
```

As expected, the marginal contributions of the two factors are 1 and 2,
respectively. For the two factors, we can manually compute the
contribution as follows:

``` r
# A:
1/2 * (simple("A") - simple()) + 1/2 * (simple(c("A", "B")) - simple("B"))
#> [1] 1
# B:
1/2 * (simple("B") - simple()) + 1/2 * (simple(c("B", "A")) - simple("A"))
#> [1] 2
```

Across the two computations, most terms occur twice. Also note that
`simple(c("A", "B")) == simple(c("B", "A"))`. The `shapley` function
only calculates each term once, and then caches the result. This leads
to great speed improvements once we consider a greater number of
factors.

## Example 1: Game theory

For this example (taken from
[Wikipedia](https://en.wikipedia.org/wiki/Shapley_value#Glove_game)),
consider three players. Players 1 and 2 supply right-hand gloves, while
Player 3 supplies a left-hand glove. The game is only successful if
players with both types of gloves enter into a coalition. We thus define
the value function as 1 if pairs `{1,3}`, `{2,3}` or `{1,2,3}` are
formed, and 0 otherwise. In R code:

``` r
glove <- function(factors) {
    if (length(factors) > 1 & 3 %in% factors) return(1)
    return(0)
}
```

To compute the marginal contributions of each player, use:

``` r
shapley(glove, c(1, 2, 3), silent = TRUE)
#>   factor     value
#> 1      1 0.1666667
#> 2      2 0.1666667
#> 3      3 0.6666667
```

## Example 2: Relative importance of predictors

Consider this simple regression model and its R<sup>2</sup>:

``` r
model <- lm(mpg ~ wt + qsec + am, data = mtcars)
summary(model)$r.squared
#> [1] 0.8496636
```

The Shapley value decomposition allows us to determine how much each
predictor contributes to the R<sup>2</sup>. To do this, we need to
define the value function in a way that it runs the regression with the
appropriate subset of predictors. It should return 0 when there are no
predictors:

``` r
reg_mtcars <- function(factors) {
    if (length(factors) == 0) return(0)
    formula <- paste("mpg ~", paste(factors, collapse = "+"))
    m <- lm(formula, data = mtcars)
    summary(m)$r.squared
}

# test - should be the same as above:
reg_mtcars(c("wt", "qsec", "am"))
#> [1] 0.8496636
```

``` r
shapley(reg_mtcars, c("wt", "qsec", "am"), silent = TRUE)
#>   factor     value
#> 1     wt 0.4792448
#> 2   qsec 0.1574791
#> 3     am 0.2129397
```

We can also generalize the value function to apply to any dataset and
dependent variable:

``` r
reg <- function(factors, dv, data) {
    if (length(factors) == 0) return(0)
    formula <- paste(dv, "~", paste(factors, collapse = "+"))
    m <- lm(formula, data = data)
    summary(m)$r.squared
}

shapley(reg, c("cyl", "hp", "am"), silent = TRUE, dv = "wt", data = mtcars)
#>   factor     value
#> 1    cyl 0.2791418
#> 2     hp 0.1960524
#> 3     am 0.2740727
```

Note that there are many packages (e.g.,
[relaimpo](https://cran.r-project.org/package=relaimpo)) that provide
this functionality specifically for regression analysis.

## Example 3: Effects of taxes and transfers on inequality

Another classic use case for the Shapley value is the decomposition of
inequality indices (see Shorrocks 2013 among others). Enami et
al. (2018) provide a simple example to show such a decomposition in the
context of measuring the impact of taxes and transfers on income
inequality.

Consider the following dataset `income`, showing the market incomes,
taxes paid, transfers received, and the resulting final incomes for five
individuals:

| MarketIncome | Tax | Transfer | FinalIncome |
| -----------: | --: | -------: | ----------: |
|            1 | \-5 |        9 |           5 |
|           20 | \-5 |        7 |          22 |
|           30 | \-5 |        5 |          30 |
|           40 | \-5 |        3 |          38 |
|           50 | \-5 |        1 |          46 |

The Gini indices of the market and final incomes are:

``` r
gini_market <- ineq::Gini(income[["MarketIncome"]])
gini_final <- ineq::Gini(income[["FinalIncome"]])
round(c(gini_market, gini_final, gini_final - gini_market), 3)
#> [1]  0.335  0.278 -0.057
```

Taxes and transfers combined thus reduced inequality by about 0.057.
There are now two different approaches to dividing this difference among
the two factors (i.e., taxes and transfers). In what Sastre and Trannoy
(2002) call the “zero income decomposition” (ZID), sources not under
consideration are set to zero. In the alternative scenario, “equalized
income decomposition” (EID), those sources are distributed evenly among
the population. Both scenarios are easily implemented using different
value functions:

``` r
zid <- function(factors, data) {
    cntf <- data[["MarketIncome"]]  # baseline for counterfactual income
    for (f in factors)
        cntf <- cntf + data[[f]]
    ineq::Gini(cntf)
}

eid <- function(factors, data) {
    cntf <- data[["MarketIncome"]]
    if ("Tax" %in% factors)
        cntf <- cntf + data[["Tax"]]
    else
        cntf <- cntf + mean(data[["Tax"]])
    if ("Transfer" %in% factors)
        cntf <- cntf + data[["Transfer"]]
    else
        cntf <- cntf + mean(data[["Transfer"]])
    ineq::Gini(cntf)
}
```

These equalities hold in both scenarios:

``` r
zid(c(), income) == gini_market
#> [1] TRUE
zid(c("Tax", "Transfer"), income) == gini_final
#> [1] TRUE
eid(c(), income) == gini_market
#> [1] TRUE
eid(c("Tax", "Transfer"), income) == gini_final
#> [1] TRUE
```

Note that for EID, the first equality only holds because the sum of
taxes and transfers is zero, i.e., those two sources cancel each other
out. Once this is no longer the case, the EID method runs into problems
(see Enami et al. for a detailed discussion). In any case, ZID and EID
give different answers when only one factor is included:

``` r
zid("Tax", income)
#> [1] 0.4068966
eid("Tax", income)
#> [1] 0.3347518
```

This is because in the zero income scenario, transfers are set to zero
when only taxes are considered, while in the equalized income scenario,
transfers are distributed equally among the individuals. The Shapley
values of the two scenarios are the following:

``` r
shapley(zid, c("Tax", "Transfer"), silent = TRUE, data = income)
#>     factor       value
#> 1      Tax  0.05700719
#> 2 Transfer -0.11374478
shapley(eid, c("Tax", "Transfer"), silent = TRUE, data = income)
#>     factor       value
#> 1      Tax  0.00000000
#> 2 Transfer -0.05673759
```

Whether ZID or EID is appropriate depends on the context. Sastre and
Trannoy (2002) and Enami et al. (2018) address this question in further
detail.

## Example 4: Hierarchical Shapley decomposition (Owen values)

Continuing from the previous example (and again borrowing from Enami et
al.), consider the case that the tax shown above is actually composed of
two different taxes, `Tax1` and `Tax2` (these two columns sum to the
column `Tax` in the previous example):

| MarketIncome | Tax1 | Tax2 | Transfer | FinalIncome |
| -----------: | ---: | ---: | -------: | ----------: |
|            1 |    0 |  \-5 |        9 |           5 |
|           20 |  \-1 |  \-4 |        7 |          22 |
|           30 |  \-2 |  \-3 |        5 |          30 |
|           40 |  \-3 |  \-2 |        3 |          38 |
|           50 |  \-4 |  \-1 |        1 |          46 |

When we now decompose this dataset (`income2`) by three factors, we get
the following results:

``` r
# we can reuse the `zid` function from above,
# while the `eid` function would need to be adapted
shapley(zid, c("Tax1", "Tax2", "Transfer"), silent = TRUE, data = income2)
#>     factor        value
#> 1     Tax1 -0.006012472
#> 2     Tax2  0.062502480
#> 3 Transfer -0.113227597
```

Note that the sum of the contributions of the two taxes does not equal
the contribution for the tax above, although this is just the sum of the
two separate taxes. Furthermore, the size of the transfer component is
affected. As Enami et al. (2018, p. 108) write:

> Given that no new tax has been added and that the only change is that
> some additional information about the sources of taxes has been
> included in the analysis, it is inconvenient that the Shapley value
> for transfers has also changed.

This is a unfortunate property of the Shapley decomposition, but it can
be partially remedied by using a hierarchical procedure, the Owen value
decomposition (Owen 1977). (An alternative is the Nested Shapley
decomposition recommended by Sastre and Trannoy (2002), which introduces
a new set of problems, though.) The `shapley` package allows the
computation of Owen values by specifying the group structure using a
list of
vectors:

``` r
shapley(zid, list(c("Tax1", "Tax2"), c("Transfer")), silent = TRUE, data = income2)
#>   group   factor       value
#> 1     1     Tax1 -0.00575388
#> 2     1     Tax2  0.06276107
#> 3     2 Transfer -0.11374478
```

Using this notation, we have grouped `Tax1` and `Tax2` together in one
group, while `Transfer` is a group in itself. The results now line up
with the results of the Shapley decomposition, where the taxes were
jointly entered as a single factor.

Note that the hierarchical procedure can also be used as an effective
tool to increase the speed of computation when a large number of factors
is included. For instance, when 8 factors are considered, 8\! = 40320
permutations need to be calculated for each factor. Once the 8 factors
are grouped into two groups with 4 factors each, the number of
permutations that need to be calculated for each factor is only 2\! \*
4\! \* 4\! = 1152.

## References

Enami, A., N. Lustig, and R. Aranda (2018). Analytic Foundations:
Measuring the Redistributive Impact of Taxes and Transfers. In: N.
Lustig (Ed.), Commitment to Equity Handbook. Estimating the Impact of
Fiscal Policy on Inequality and Poverty, Washington, D.C.: Brookings,
56-115.

Owen, G. (1977). Values of Games with a Priori Unions. In: R. Henn and
O. Moeschlin (Eds.): Mathematical Economics and Game Theory, Berlin and
Heidelberg: Springer, 76-88.

Sastre, M. and A. Trannoy (2002). Shapley Inequality Decomposition by
Factor Components: Some Methodological Issues. Journal of Economics
77(1): 51-89. <https://doi.org/10.1007/BF03052500>

Shapley, L. S. (1953). A value for n-person games. In: A. W. Tucker and
H. W. Kuhn (Eds.), Contributions to the theory of games (Vol. II),
Princeton: Princeton University Press, 307–317.

Shorrocks, A. F. (2013). Decomposition procedures for distributional
analysis: a unified framework based on the Shapley value. Journal of
Economic Inequality 11: 1-28.
<https://doi.org/10.1007/s10888-011-9214-z>
