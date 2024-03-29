#' @title Calculate UK income and/or National Insurance taxes
#'
#' @description This function applies the prevailing tax bands and rates to give a simple calculation for UK Income Tax and National Insurance (to the nearest couple of pounds). Please note that these are approximations, so do not rely on them for financial returns or planning. The output is a list, containing the following measures: `income_net` for the net income (after Income Tax and National Insurance); `income_tax` for the Income Tax liability; `ni` for the National Insurance liability; and `total_tax` for the combined Income Tax and National Insurance liability.
#' @param income_taxable The taxable income level (i.e. after deductions for things like pension contributions).
#' @param tax_year_end The calendar year in which the tax year ends, as a YYYY integer. For example, tax year 2023/24 would be 2024.
#' @keywords Tax
#' @export
#' @examples
#' liability_tax(
#'   income_taxable = 38000, 
#'   tax_year_end = 2024
#'   )$total_tax
#' 
#' @importFrom rlang .data

liability_tax <- function(income_taxable, tax_year_end){
  
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
      income_net = income_taxable - .data$tax - .data$ni,
      taxes_total = .data$tax + .data$ni
      )
  
  # Output ----
  output_income_net <- tax_calculations |> 
    dplyr::pull(.data$income_net)
  
  output_tax <- tax_calculations |> 
    dplyr::pull(.data$tax)
  
  output_ni <- tax_calculations |> 
    dplyr::pull(.data$ni)
  
  output_taxes_total <- tax_calculations |> 
    dplyr::pull(.data$taxes_total)
  
  output_values <- list("income_net" = output_income_net, "income_tax" = output_tax, "ni" = output_ni, "total_tax" = output_taxes_total)
  
  return(output_values)
}