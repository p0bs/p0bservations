test_that("error checks work", {

  expect_snapshot(
    error = TRUE,
    rate_annuity(
      value_age = 55,
      value_specifications = "single_simple"
    )
  )

  expect_snapshot(
    error = TRUE,
    rate_annuity(
      value_age = "55",
      value_specifications = "single"
    )
  )

  expect_snapshot(
    error = TRUE,
    rate_annuity(
      value_age = "55"
    )
  )

  expect_snapshot(
    error = TRUE,
    rate_annuity(
      value_specifications = "single_simple"
    )
  )

})
