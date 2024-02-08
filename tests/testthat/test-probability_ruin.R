test_that("error checks work", {
  
  expect_error(
    probability_ruin(
      return_expected = -0.05, 
      return_sd = 0.1, 
      life_remaining_expected = 22, 
      rate_spend = 0.05
      )
    )
  
  expect_error(
    probability_ruin(
      return_expected = 0.05, 
      return_sd = 0.1, 
      life_remaining_expected = "thirty years", 
      rate_spend = 0.05
    )
  )
  
})

test_that("calculations work", {
  
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