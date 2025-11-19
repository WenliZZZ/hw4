# hw4: Logistic Regression via IRLS

[![test-coverage](https://github.com/WenliZZZ/hw4/workflows/test-coverage/badge.svg)](https://github.com/WenliZZZ/hw4/actions)

## Introduction

This package fits a binary logistic regression model using the IRLS algorithm. It was created for Biostat 625 to practice modeling, numerical optimization, and basic R package development.

## Features

- Logistic regression via IRLS algorithm
- Core function: `my_logit()`
- S3 methods: `print()`, `summary()`, `residuals()`
- Convergence tracking and iteration count
- Fitted values, deviance residuals, variance–covariance matrix
- Unit testing using `testthat`
- Vignette with correctness comparison and benchmark results
- Continuous integration using GitHub Actions (test coverage badge)

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("WenliZZZ/hw4", build_vignettes = TRUE)
```

## Usage

```r
library(hw4)

fit <- my_logit(am ~ mpg + wt, data = mtcars)

print(fit)
summary(fit)
residuals(fit)[1:5]
```

## Correctness Verification

```r
fit_my  <- my_logit(am ~ mpg + wt, data = mtcars)
fit_glm <- glm(am ~ mpg + wt, data = mtcars, family = binomial())

all.equal(as.vector(fit_my$coefficients),
          as.vector(coef(fit_glm)),
          tolerance = 1e-6)

all.equal(as.vector(fit_my$fitted.values),
          as.vector(fitted(fit_glm)),
          tolerance = 1e-6)

all.equal(as.vector(fit_my$se),
          as.vector(summary(fit_glm)$coefficients[, "Std. Error"]),
          tolerance = 1e-4)
```

These comparisons confirm that `my_logit()` produces results equivalent to `glm()`.


## Algorithm Overview (IRLS)

1. Initialize coefficients β = 0
2. Compute linear predictors η = Xβ
3. Calculate predicted probabilities μ = 1 / (1 + exp(-η))
4. Compute weights W = μ(1 − μ)
5. Update coefficients using weighted least squares:
   β_new = (X'WX)^(-1) X'Wz
6. Repeat until convergence


## Vignette

A detailed vignette including usage, correctness comparison, and benchmark results is available:

```r
vignette("my_logit_vignette", package = "hw4")
```

## Testing

I used testthat to check:
- coefficients and fitted probabilities match glm()
- estimated standard errors are correct
- residuals are numeric and have correct length
- model converges
- formula is correctly stored in the object

All tests passed locally.

## Project Structure

The package is organized using the typical R package structure.  
The implementation of `my_logit()` and its S3 methods is inside the `R/` folder, the help files are stored in `man/`, and the unit tests are placed in `tests/testthat/`.  
I wrote a vignette in `vignettes/` to show examples and comparison with glm(), and I also set up GitHub Actions (R-CMD-check and coverage) under `.github/workflows/` to automate testing.


## Dependencies

The package only relies on base R, and functions from the stats package are used in the implementation.
For development, I used testthat to write unit tests, and knitr and rmarkdown to build the vignette.
I also included bench in the vignette to compare the performance of my_logit with glm, but it is not needed for normal use.
