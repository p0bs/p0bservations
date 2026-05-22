test_that("error checks work", {

  expect_snapshot(
    error = TRUE,
    liability_tax(
      income_taxable = "15000",
      tax_year_end = 2024L
    )
  )

  expect_snapshot(
    error = TRUE,
    liability_tax(
      income_taxable = -20000,
      tax_year_end = 2024
    )
  )

  expect_snapshot(
    error = TRUE,
    liability_tax(
      income_taxable = 38000,
      tax_year_end = c(2024, 2025)
    )
  )

})

test_that("scalar calculations work", {

  expect_equal(
    object = liability_tax(
      income_taxable = 8000,
      tax_year_end = 2024
    )$total_tax,
    expected = 0,
    tolerance = 0.001
  )

  # Calculations validated against a third-party tax app
  expect_equal(
    object = liability_tax(
      income_taxable = 22000,
      tax_year_end = 2024
    )$income_net,
    expected = 18984.08,
    tolerance = 0.001
  )

  expect_equal(
    object = liability_tax(
      income_taxable = 40000,
      tax_year_end = 2024
    )$total_tax,
    expected = 40000 - 31224.08,
    tolerance = 0.001
  )

  expect_equal(
    object = liability_tax(
      income_taxable = 65000,
      tax_year_end = 2024
    )$total_tax,
    expected = 65000 - 46749.68,
    tolerance = 0.001
  )

  expect_equal(
    object = liability_tax(
      income_taxable = 105000,
      tax_year_end = 2024
    )$total_tax,
    expected = 105000 - 68949.68,
    tolerance = 0.001
  )

  expect_equal(
    object = liability_tax(
      income_taxable = 160000,
      tax_year_end = 2024
    )$total_tax,
    expected = 160000 - 95078,
    tolerance = 0.001
  )

})

test_that("vectorised calculation works", {

  result <- liability_tax(
    income_taxable = c(22000, 40000),
    tax_year_end = 2024
  )

  expect_length(result$income_net, 2)
  expect_equal(result$income_net[[1]], 18984.08, tolerance = 0.001)
  expect_equal(result$total_tax[[2]], 40000 - 31224.08, tolerance = 0.001)

})
