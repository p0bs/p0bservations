## code to prepare `tax_parameters` dataset goes here
tax_parameters <- tibble::tibble(
  year_tax_end = c(2021L, 2022L, 2023L),
  level_ni_lower = c(9500, 9568, 12570), # Ignored change in opening months of FY22/23
  level_ni_upper = c(50000, 50270, 50270),
  level_allowance_lower = c(12500, 12570, 12570),
  level_allowance_upper = c(100000, 100000, 100000),
  level_tax_higher = c(50000, 50270, 50270),
  level_tax_upper = c(150000, 150000, 150000),
  rate_ni_lower = c(0.12, 0.12, 0.1325),
  rate_ni_upper = c(0.02, 0.02, 0.0325),
  rate_allowance_drop = c(0.5, 0.5, 0.5),
  rate_tax_basic = c(0.2, 0.2, 0.2),
  rate_tax_higher = c(0.4, 0.4, 0.4),
  rate_tax_upper = c(0.45, 0.45, 0.45),
  rate_tax_sales = c(0.2, 0.2, 0.2)
)

usethis::use_data(tax_parameters, overwrite = TRUE)
