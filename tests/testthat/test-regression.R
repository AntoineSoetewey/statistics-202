library(testthat)

# --- Coefficient estimates ---

test_that("lm recovers known intercept and slope", {
  x <- c(1, 2, 3, 4, 5)
  y <- 2 + 3 * x
  fit <- lm(y ~ x)
  expect_equal(unname(coef(fit)[1]), 2)
  expect_equal(unname(coef(fit)[2]), 3)
})

test_that("beta1 matches manual formula", {
  x <- c(90, 100, 90, 80, 87, 75)
  y <- c(950, 1000, 850, 750, 950, 775)
  fit <- lm(y ~ x)
  n <- length(x)
  beta1_manual <- (sum(x * y) - n * mean(x) * mean(y)) / sum((x - mean(x))^2)
  expect_equal(unname(coef(fit)[2]), beta1_manual, tolerance = 1e-10)
})

test_that("beta0 equals ybar - beta1 * xbar", {
  x <- c(90, 100, 90, 80, 87, 75)
  y <- c(950, 1000, 850, 750, 950, 775)
  fit <- lm(y ~ x)
  beta0_manual <- mean(y) - coef(fit)[2] * mean(x)
  expect_equal(unname(coef(fit)[1]), unname(beta0_manual), tolerance = 1e-10)
})

# --- Fitted values and residuals ---

test_that("fitted values equal beta0 + beta1 * x", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2.1, 4.9, 6.2, 8.0, 10.1)
  fit <- lm(y ~ x)
  expected <- coef(fit)[1] + coef(fit)[2] * x
  expect_equal(unname(fitted(fit)), unname(expected), tolerance = 1e-10)
})

test_that("residuals sum to zero", {
  x <- c(90, 100, 90, 80, 87, 75)
  y <- c(950, 1000, 850, 750, 950, 775)
  fit <- lm(y ~ x)
  expect_equal(sum(resid(fit)), 0, tolerance = 1e-10)
})

test_that("residuals equal observed minus fitted", {
  x <- c(1, 3, 5, 7, 9)
  y <- c(2, 5, 7, 10, 13)
  fit <- lm(y ~ x)
  expect_equal(unname(resid(fit)), unname(y - fitted(fit)), tolerance = 1e-10)
})

# --- R-squared ---

test_that("R-squared is between 0 and 1", {
  x <- c(90, 100, 90, 80, 87, 75)
  y <- c(950, 1000, 850, 750, 950, 775)
  fit <- lm(y ~ x)
  r2 <- summary(fit)$r.squared
  expect_gte(r2, 0)
  expect_lte(r2, 1)
})

test_that("R-squared equals 1 for a perfect linear relationship", {
  x <- 1:10
  y <- 5 + 2 * x
  fit <- lm(y ~ x)
  expect_equal(summary(fit)$r.squared, 1)
})

test_that("adjusted R-squared is less than or equal to R-squared", {
  x <- c(90, 100, 90, 80, 87, 75)
  y <- c(950, 1000, 850, 750, 950, 775)
  fit <- lm(y ~ x)
  expect_lte(summary(fit)$adj.r.squared, summary(fit)$r.squared)
})

# --- Prediction ---

test_that("predict returns correct value for a new observation", {
  x <- c(1, 2, 3, 4, 5)
  y <- 2 + 3 * x
  fit <- lm(y ~ x)
  pred <- predict(fit, newdata = data.frame(x = 6))
  expect_equal(unname(pred), 20, tolerance = 1e-10)
})

# --- Input validation checks (replicating server logic) ---

test_that("at least 3 observations are required", {
  x <- c(1, 2)
  y <- c(3, 4)
  expect_true(length(x) < 3)
})

test_that("x and y must have the same length", {
  x <- c(1, 2, 3)
  y <- c(4, 5)
  expect_false(length(x) == length(y))
})

test_that("x must have more than one distinct value", {
  x <- c(5, 5, 5)
  expect_equal(length(unique(x)), 1)
})
