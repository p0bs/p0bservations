test_that("error checks work", {

  expect_snapshot(
    error = TRUE,
    probability_ruin(
      return_expected = -0.05,
      return_sd = 0.1,
      life_remaining_expected = 22,
      rate_spend = 0.05
    )
  )

  expect_snapshot(
    error = TRUE,
    probability_ruin(
      return_expected = 0.05,
      return_sd = 0.1,
      life_remaining_expected = "thirty years",
      rate_spend = 0.05
    )
  )

})

test_that("scalar calculation works", {

  expect_equal(
    object = probability_ruin(
      return_expected = 0.07,
      return_sd = 0.2,
      life_remaining_expected = 28.1,
      rate_spend = 0.05
    ),
    expected = 0.268,
    tolerance = 0.001
  )

})

test_that("vectorised calculation works", {

  result <- probability_ruin(
    return_expected = 0.07,
    return_sd = 0.2,
    life_remaining_expected = 28.1,
    rate_spend = c(0.03, 0.05, 0.07)
  )

  expect_length(result, 3)
  expect_equal(result[[2]], 0.268, tolerance = 0.001)
  # Higher spend rate should produce higher ruin probability
  expect_true(result[[1]] < result[[2]])
  expect_true(result[[2]] < result[[3]])

})
