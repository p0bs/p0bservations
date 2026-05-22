test_that("error checks work", {

  expect_snapshot(
    error = TRUE,
    probability_ruin_rate(
      return_expected = "high",
      return_sd = 0.1,
      life_remaining_expected = 28.1,
      value_probability_ruin = 0.1
    )
  )

  expect_snapshot(
    error = TRUE,
    probability_ruin_rate(
      return_expected = -0.04,
      return_sd = 0.1,
      life_remaining_expected = 28.1,
      value_probability_ruin = 0.1
    )
  )

  expect_snapshot(
    error = TRUE,
    probability_ruin_rate(
      return_expected = 0.04,
      return_sd = 0.1,
      life_remaining_expected = 28.1,
      value_probability_ruin = 0
    )
  )

  expect_snapshot(
    error = TRUE,
    probability_ruin_rate(
      return_expected = 0.04,
      return_sd = 0.1,
      life_remaining_expected = 28.1,
      value_probability_ruin = 1.2
    )
  )

})

test_that("scalar calculations work", {

  expect_equal(
    object = probability_ruin_rate(
      return_expected = 0.03574065,
      return_sd = 0.06383427,
      life_remaining_expected = 31,
      value_probability_ruin = 0.1
    ),
    expected = 0.03296928,
    tolerance = 0.002
  )

  expect_equal(
    object = probability_ruin_rate(
      return_expected = 0.04,
      return_sd = 0.1,
      life_remaining_expected = 28.1,
      value_probability_ruin = 0.1
    ),
    expected = 0.0320,
    tolerance = 0.002
  )

})

test_that("vectorised calculation works", {

  result <- probability_ruin_rate(
    return_expected = 0.07,
    return_sd = 0.2,
    life_remaining_expected = 28.1,
    value_probability_ruin = c(0.05, 0.1, 0.2)
  )

  expect_length(result, 3)
  # Higher target ruin probability should yield higher spend rate
  expect_true(result[[1]] < result[[2]])
  expect_true(result[[2]] < result[[3]])

})

test_that("probability_ruin_rate is the inverse of probability_ruin", {

  rate <- probability_ruin_rate(
    return_expected = 0.07,
    return_sd = 0.2,
    life_remaining_expected = 28.1,
    value_probability_ruin = 0.1
  )

  recovered <- probability_ruin(
    return_expected = 0.07,
    return_sd = 0.2,
    life_remaining_expected = 28.1,
    rate_spend = rate
  )

  expect_equal(recovered, 0.1, tolerance = 1e-10)

})
