test_that("calculations work", {
  
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