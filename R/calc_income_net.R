#' @title Calculate income net of UK tax and National Insurance
#'
#' @description This function applies the prevailing tax bands and rates to give a simple calculation for income after UK tax and National Insurance (to the nearest couple of pounds) for FY20/21. Please note that these are approximations, so do not rely on them for financial returns or planning.
#' @param income_taxable The taxable income level (i.e. after deductions for things like pension contributions).
#' @param tax_year_end The calendar year in which the tax year ends, as a YYYY integer. For example, tax year 2020/21 would be 2021L (which is the default value).
#' @keywords Tax
#' @export
#' @examples
#' \dontrun{
#' calc_income_net(income_taxable = 38000, tax_year_end = 2024L)
#' }
#' 
#' @importFrom rlang .data

calc_income_net <- function(income_taxable, tax_year_end){
  
  # Error checks ----
  stop_not_scalar_double(income_taxable)
  stop_not_scalar_double(tax_year_end)
  stop_not_positive(income_taxable)
  
  year_tax_end_options <- p0bservations::tax_parameters |> 
    dplyr::distinct(.data$year_tax_end) |> 
    dplyr::pull(.data$year_tax_end)
  
  if (!(tax_year_end %in% year_tax_end_options)){
    rlang::abort("tax_year_end is not in our dataset. Please choose another entry.")
  }
  
  
  # Add tax limits, rates and breakpoints ----
  parameters_FY <- p0bservations::tax_parameters |> 
    dplyr::filter(.data$year_tax_end == tax_year_end)
  

  # Tax calculations ----
  tax_calculations <- parameters_FY |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      income_allowance = max(
        0, 
        .data$level_allowance_lower - 0.5 * max(
          income_taxable - .data$level_allowance_upper, 
          0
          )
        ),
      taxable_ni_lower = max(
        0, 
        (
          min(
            income_taxable - .data$level_ni_lower, 
            .data$level_ni_upper - .data$level_ni_lower
            )
          )
        ),
      taxable_ni_higher = max(
        0, 
        (income_taxable - .data$level_ni_upper)
        ),
      allowance_drop = .data$rate_allowance_drop * max(
        income_taxable - .data$level_allowance_upper, 
        0
        ),
      taxable_zero = max(
        0, 
        .data$level_allowance_lower - max(
          .data$allowance_drop, 
          0
          )
        ),
      excess_zero = max(
        0, 
        (income_taxable - .data$taxable_zero)
        ),
      taxable_basic = min(
        .data$excess_zero, 
        (.data$level_tax_higher - .data$level_allowance_lower)
        ),
      excess_basic = max(
        0, 
        (income_taxable - .data$taxable_zero - .data$taxable_basic)
        ),
      taxable_higher = min(
        .data$excess_basic, 
        (.data$level_tax_upper - .data$level_tax_higher + .data$level_allowance_lower)
        ),
      taxable_additional = max(
        0, 
        (income_taxable - .data$taxable_zero - .data$taxable_basic - .data$taxable_higher)
        ),
      tax = (.data$rate_tax_basic * .data$taxable_basic) + 
        (.data$rate_tax_higher * .data$taxable_higher) + 
        (.data$rate_tax_upper * .data$taxable_additional),
      ni = (.data$rate_ni_lower * .data$taxable_ni_lower) + 
        (.data$rate_ni_upper * .data$taxable_ni_higher),
      income_net = income_taxable - .data$tax - .data$ni
      )
  
  # Output ----
  output_income_net <- tax_calculations |> 
    dplyr::pull(.data$income_net)
  
  output_tax <- tax_calculations |> 
    dplyr::pull(.data$tax)
  
  output_ni <- tax_calculations |> 
    dplyr::pull(.data$ni)
  
  output_ni_lower <- tax_calculations |> 
    dplyr::pull(.data$taxable_ni_lower)
  
  output_ni_higher <- tax_calculations |> 
    dplyr::pull(.data$taxable_ni_higher)
  
  # return(list(output_income_net, output_tax, output_ni, output_ni_lower, output_ni_higher))
  return(output_income_net)
}