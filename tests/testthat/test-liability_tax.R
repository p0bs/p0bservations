test_that("error checks work", {
  
  expect_error(
    liability_tax(
      income_taxable = "15000",
      tax_year_end = 2024L
    )
  )

  expect_error(
    liability_tax(
      income_taxable = c(9000, 29000),
      tax_year_end = 2024
    )
  )
  
  expect_error(
    liability_tax(
      income_taxable = -20000,
      tax_year_end = 2024
    )
  )
  
})

test_that("calculations work", {
  
  expect_true(
    is.numeric(
      liability_tax(
        income_taxable = 20000,
        tax_year_end = 2024
      )
    )
  )
  
  expect_equal(
    object = liability_tax(
      income_taxable = 8000,
      tax_year_end = 2024
      ), 
    expected = 8000 - 8000, 
    tolerance = 0.001
  )
  
  # Calculations taken from a third-party app
  
  expect_equal(
    object = liability_tax(
      income_taxable = 22000, 
      tax_year_end = 2024
    ),
    expected = 22000 - 18984.08, 
    tolerance = 0.001
  )
  
  expect_equal(
    object = liability_tax(
      income_taxable = 40000, 
      tax_year_end = 2024
      ),
    expected = 40000 - 31224.08, 
    tolerance = 0.001
  )
  
  expect_equal(
    object = liability_tax(
      income_taxable = 65000, 
      tax_year_end = 2024
    ),
    expected = 65000 - 46749.68, 
    tolerance = 0.001
  )
  
  expect_equal(
    object = liability_tax(
      income_taxable = 105000, 
      tax_year_end = 2024
    ),
    expected = 105000 - 68949.68, 
    tolerance = 0.001
  )
  
  expect_equal(
    object = liability_tax(
      income_taxable = 160000, 
      tax_year_end = 2024
    ),
    expected = 160000 - 95078, 
    tolerance = 0.001
  )
  
})