test_that("check_numeric errors on non-numeric input", {
  expect_snapshot(error = TRUE, check_numeric("text"))
  expect_snapshot(error = TRUE, check_numeric(TRUE))
})

test_that("check_numeric passes for numeric input", {
  expect_no_error(check_numeric(1.5))
  expect_no_error(check_numeric(c(1, 2, 3)))
})

test_that("check_positive errors on zero or negative values", {
  expect_snapshot(error = TRUE, check_positive(0))
  expect_snapshot(error = TRUE, check_positive(-1))
  expect_snapshot(error = TRUE, check_positive(c(1, -2, 3)))
})

test_that("check_positive passes for positive values", {
  expect_no_error(check_positive(0.001))
  expect_no_error(check_positive(c(1, 2, 3)))
})

test_that("check_probability errors on out-of-range values", {
  expect_snapshot(error = TRUE, check_probability(0))
  expect_snapshot(error = TRUE, check_probability(1))
  expect_snapshot(error = TRUE, check_probability(1.5))
  expect_snapshot(error = TRUE, check_probability(-0.1))
})

test_that("check_probability passes for valid probabilities", {
  expect_no_error(check_probability(0.5))
  expect_no_error(check_probability(c(0.1, 0.5, 0.9)))
})

test_that("check_scalar_integerish errors on non-scalar or non-integer input", {
  expect_snapshot(error = TRUE, check_scalar_integerish(c(1L, 2L)))
  expect_snapshot(error = TRUE, check_scalar_integerish("2024"))
  expect_snapshot(error = TRUE, check_scalar_integerish(NA_integer_))
})

test_that("check_scalar_integerish passes for single integer-like values", {
  expect_no_error(check_scalar_integerish(2024L))
  expect_no_error(check_scalar_integerish(2024))
})
