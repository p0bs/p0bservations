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
      income_taxable = 8000,
      tax_year_end = 2024L
      ), 
    expected = 8000, 
    tolerance = 0.0001
  )
  
  # Calculations taken from a third-party app
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 22000, 
      tax_year_end = 2024L
    ),
    expected = 18984.08, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 40000, 
      tax_year_end = 2024L
      ),
    expected = 31224.08, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 65000, 
      tax_year_end = 2024L
    ),
    expected = 46749.68, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 105000, 
      tax_year_end = 2024L
    ),
    expected = 68949.68, 
    tolerance = 0.0001
  )
  
  expect_equal(
    object = calc_income_net(
      income_taxable = 160000, 
      tax_year_end = 2024L
    ),
    expected = 95078, 
    tolerance = 0.0001
  )
  
})