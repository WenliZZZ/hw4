test_that("coefficients match glm", {
  fit_my  <- my_logit(am ~ mpg + wt, data = mtcars)
  fit_glm <- glm(am ~ mpg + wt, data = mtcars, family = binomial())
  expect_equal(as.vector(fit_my$coefficients),
               as.vector(coef(fit_glm)),
               tolerance = 1e-6)
})

test_that("fitted values match glm", {
  fit_my  <- my_logit(am ~ mpg + wt, data = mtcars)
  fit_glm <- glm(am ~ mpg + wt, data = mtcars, family = binomial())
  expect_equal(as.vector(fit_my$fitted.values),
               as.vector(fitted(fit_glm)),
               tolerance = 1e-6)
})

test_that("standard errors match glm", {
  fit_my  <- my_logit(am ~ mpg + wt, data = mtcars)
  fit_glm <- glm(am ~ mpg + wt, data = mtcars, family = binomial())
  expect_equal(as.vector(fit_my$se),
               as.vector(summary(fit_glm)$coefficients[, "Std. Error"]),
               tolerance = 1e-4)
})

test_that("residuals match length and are numeric", {
  fit_my <- my_logit(am ~ mpg + wt, data = mtcars)
  expect_true(is.numeric(residuals(fit_my)))
  expect_equal(length(residuals(fit_my)), nrow(mtcars))
})

test_that("formula is correctly stored", {
  fit_my <- my_logit(am ~ mpg + wt, data = mtcars)
  expect_equal(as.character(fit_my$formula),
               as.character(am ~ mpg + wt))
})

test_that("model converged", {
  fit_my <- my_logit(am ~ mpg + wt, data = mtcars)
  expect_true(fit_my$converged)
})
