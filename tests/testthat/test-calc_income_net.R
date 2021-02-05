test_that("error checks work", {
  
  expect_error(
    calc_income_net(
      income_taxable = "15000"
    ), 
    regexp = "income_taxable must be a numeric input"
  )

  expect_error(
    calc_income_net(
      income_taxable = c(9000, 29000)
    ), 
    regexp = "You must enter one, and only one, entry for income_taxable"
  )
  
  expect_error(
    calc_income_net(
      income_taxable = -20000
    ), 
    regexp = "income_taxable must be positive"
  )
  
})

test_that("calculations work", {
  
  expect_true(
    is.numeric(
      calc_income_net(
        income_taxable = 20000
      )
    )
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 8000
      ), 
    expected = 8000, 
    tolerance = 0.0001
  )
  
  # Calculations taken from a third-party app
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 22000
    ),
    expected = 18601, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 40000
      ),
    expected = 30842, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 65000
    ),
    expected = 46340, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 105000
    ),
    expected = 68540, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 160000
    ),
    expected = 95940, 
    tolerance = 0.0001
  )
  
})