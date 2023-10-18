## code to prepare `tax_parameters` dataset goes here
tax_parameters <- tibble::tibble(
  year_tax_end = c(2024L),   # Tax year ending April of this year
  level_ni_lower = c(12570), 
  level_ni_upper = c(50270),
  level_allowance_lower = c(12570),
  level_allowance_upper = c(100000),
  level_tax_higher = c(50270),
  level_tax_upper = c(125140),
  rate_ni_lower = c(0.12),
  rate_ni_upper = c(0.02),
  rate_allowance_drop = c(0.5),
  rate_tax_basic = c(0.2),
  rate_tax_higher = c(0.4),
  rate_tax_upper = c(0.45),
  rate_tax_sales = c(0.2)
)

usethis::use_data(tax_parameters, overwrite = TRUE)
