library(testthat)

source(file.path("..", "..", "helpers.R"))

# --- extract ---

test_that("extract parses comma-separated integers", {
  expect_equal(extract("1, 2, 3"), c(1, 2, 3))
})

test_that("extract parses comma-separated decimals", {
  expect_equal(extract("4.2, 4.4, 5, 5.03"), c(4.2, 4.4, 5, 5.03))
})

test_that("extract strips spaces", {
  expect_equal(extract("10 , 20 , 30"), c(10, 20, 30))
})

test_that("extract returns NA for non-numeric input", {
  expect_true(all(is.na(extract("a, b, c"))))
})

test_that("extract returns NA for mixed valid/invalid input", {
  result <- extract("1, two, 3")
  expect_equal(result[1], 1)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 3)
})

test_that("extract handles negative values", {
  expect_equal(extract("-1, -2.5, 3"), c(-1, -2.5, 3))
})

test_that("extract returns a single value for a single input", {
  expect_equal(extract("42"), 42)
})

# --- validate_inputs ---

test_that("validate_inputs returns NULL for valid inputs", {
  expect_null(validate_inputs(c(1, 2, 3), c(4, 5, 6)))
})

test_that("validate_inputs rejects fewer than 3 observations in x", {
  err <- validate_inputs(c(1, 2), c(3, 4))
  expect_match(err, "at least 3 required")
})

test_that("validate_inputs rejects fewer than 3 observations in y", {
  err <- validate_inputs(c(1, 2, 3), c(4, 5))
  expect_match(err, "at least 3 required")
})

test_that("validate_inputs rejects NA values in x", {
  err <- validate_inputs(c(1, NA, 3), c(4, 5, 6))
  expect_match(err, "at least 3 required")
})

test_that("validate_inputs rejects NA values in y", {
  err <- validate_inputs(c(1, 2, 3), c(4, NA, 6))
  expect_match(err, "at least 3 required")
})

test_that("validate_inputs rejects unequal lengths of x and y", {
  err <- validate_inputs(c(1, 2, 3), c(4, 5, 6, 7))
  expect_match(err, "equal for x and y")
})

test_that("validate_inputs rejects x with only one distinct value", {
  err <- validate_inputs(c(5, 5, 5), c(1, 2, 3))
  expect_match(err, "more than one distinct value")
})

test_that("validate_inputs accepts x with two or more distinct values", {
  expect_null(validate_inputs(c(1, 1, 2), c(3, 4, 5)))
})
