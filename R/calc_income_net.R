#' @title Calculate income net of UK tax and National Insurance
#'
#' @description This function applies the prevailing tax bands and rates to give a simple calculation for income after UK tax and National Insurance (to the nearest couple of pounds) for FY20/21.
#' @param income_taxable The taxable income level (i.e. after deductions for things like pension contributions).
#' @keywords Tax
#' @export
#' @examples
#' \dontrun{
#' calc_income_net(income_taxable = 38000)
#' }

calc_income_net <- function(income_taxable){
  
  if (!is.numeric(income_taxable)){ 
    rlang::abort("income_taxable must be a numeric input") 
  } 
  
  if (length(income_taxable) != 1){ 
    rlang::abort("You must enter one, and only one, entry for income_taxable") 
  } 
  
  if (income_taxable <= 0){
    rlang::abort("income_taxable must be positive")
  }
  
  # Add tax limits, rates and breakpoints
  level_ni_lower <- 9500
  level_ni_upper <- 50000
  level_allowance_lower <- 12500
  level_allowance_upper <- 100000
  level_tax_higher <- 50000
  level_tax_upper <- 150000
  rate_ni_lower <- 0.12
  rate_ni_upper <- 0.02
  rate_allowance_drop <- 0.5
  rate_tax_basic <- 0.2
  rate_tax_higher <- 0.4
  rate_tax_upper <- 0.45
  rate_tax_sales <- 0.2
  
  # Tax calculations
  income_allowance <- max(
    0, 
    level_allowance_lower - 0.5 * max(
      income_taxable - level_allowance_upper, 
      0
      )
    )
  
  taxable_ni_lower <- max(
    0, 
    (min(
      income_taxable, 
      level_ni_upper
      ) - level_ni_lower)
    )
  taxable_ni_higher <- max(
    0, 
    (income_taxable - level_ni_upper)
    )
  
  allowance_drop <- rate_allowance_drop * max(
    income_taxable - level_allowance_upper, 
    0
    )
  
  taxable_zero <- max(
    0, 
    level_allowance_lower - max(
      allowance_drop, 
      0
      )
    )
  
  excess_zero <- max(
    0, 
    (income_taxable - taxable_zero)
    )
  
  taxable_basic <- min(
    excess_zero, 
    (level_tax_higher - level_allowance_lower)
    )
  
  excess_basic <- max(
    0, 
    (income_taxable - taxable_zero - taxable_basic)
    )
  
  taxable_higher <- min(
    excess_basic, 
    (level_tax_upper - level_tax_higher + level_allowance_lower)
    )
  
  taxable_additional <- max(
    0, 
    (income_taxable - taxable_zero - taxable_basic - taxable_higher)
    )
  
  tax <- (rate_tax_basic * taxable_basic) + 
    (rate_tax_higher * taxable_higher) + 
    (rate_tax_upper * taxable_additional)
  
  ni <- (rate_ni_lower * taxable_ni_lower) + 
    (rate_ni_upper * taxable_ni_higher)
  
  income_net <- income_taxable - tax - ni
  
  return(income_net)
  
}