library(testthat)

source(file.path("..", "..", "helpers.R"))

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
  result <- extract("a, b, c")
  expect_true(all(is.na(result)))
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
