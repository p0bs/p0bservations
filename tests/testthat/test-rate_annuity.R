test_that("error checks work", {
  
  expect_error(
    rate_annuity(
      value_age = 55, 
      value_specifications = "single_simple"
    )
  )
  
  expect_error(
    rate_annuity(
      value_age = "55", 
      value_specifications = "single"
    )
  )
  
  expect_error(
    rate_annuity(
      value_age = "55"
    )
  )
  
  expect_error(
    rate_annuity(
      value_specifications = "single_simple"
    )
  )

})
