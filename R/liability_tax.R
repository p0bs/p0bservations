#' @title Calculate UK income and/or National Insurance taxes
#'
#' @description This function applies the prevailing tax bands and rates to
#' give a simple calculation for UK Income Tax and National Insurance (to the
#' nearest couple of pounds). Please note that these are approximations, so do
#' not rely on them for financial returns or planning. The output is a named
#' list containing: `income_net` for the net income after Income Tax and
#' National Insurance; `income_tax` for the Income Tax liability; `ni` for the
#' National Insurance liability; and `total_tax` for the combined liability.
#' `income_taxable` may be a numeric vector; all output list elements will be
#' vectors of the same length.
#' @param income_taxable The taxable income level (i.e. after deductions for
#'   things like pension contributions). May be a numeric vector.
#' @param tax_year_end The calendar year in which the tax year ends, as a YYYY
#'   integer. For example, tax year 2023/24 would be 2024.
#' @keywords Tax
#' @export
#' @examples
#' liability_tax(
#'   income_taxable = 38000,
#'   tax_year_end = 2024
#' )$total_tax
#'
#' # Vectorised over multiple incomes
#' liability_tax(
#'   income_taxable = c(22000, 40000, 65000),
#'   tax_year_end = 2024
#' )$total_tax
#'
#' @importFrom rlang .data

liability_tax <- function(income_taxable, tax_year_end) {

  # Error checks ----
  check_numeric(income_taxable)
  check_positive(income_taxable)
  check_scalar_integerish(tax_year_end)

  year_tax_end_options <- tax_parameters |>
    dplyr::distinct(.data$year_tax_end) |>
    dplyr::pull(.data$year_tax_end)

  if (!(tax_year_end %in% year_tax_end_options)) {
    rlang::abort(
      "tax_year_end is not in our dataset. Please choose another entry.",
      class = "p0bservations_error_invalid_year"
    )
  }

  # Extract parameters for the requested tax year ----
  p <- tax_parameters |>
    dplyr::filter(.data$year_tax_end == tax_year_end) |>
    as.list()

  # Tax calculations (vectorised over income_taxable) ----
  allowance_drop     <- p$rate_allowance_drop * pmax(income_taxable - p$level_allowance_upper, 0)
  taxable_zero       <- pmax(0, p$level_allowance_lower - pmax(allowance_drop, 0))
  excess_zero        <- pmax(0, income_taxable - taxable_zero)
  taxable_basic      <- pmin(excess_zero, p$level_tax_higher - p$level_allowance_lower)
  excess_basic       <- pmax(0, income_taxable - taxable_zero - taxable_basic)
  taxable_higher     <- pmin(
    excess_basic,
    p$level_tax_upper - p$level_tax_higher + p$level_allowance_lower
  )
  taxable_additional <- pmax(
    0,
    income_taxable - taxable_zero - taxable_basic - taxable_higher
  )
  taxable_ni_lower   <- pmax(
    0,
    pmin(income_taxable - p$level_ni_lower, p$level_ni_upper - p$level_ni_lower)
  )
  taxable_ni_higher  <- pmax(0, income_taxable - p$level_ni_upper)

  income_tax <- p$rate_tax_basic  * taxable_basic  +
                p$rate_tax_higher * taxable_higher  +
                p$rate_tax_upper  * taxable_additional
  ni         <- p$rate_ni_lower * taxable_ni_lower +
                p$rate_ni_upper * taxable_ni_higher
  income_net <- income_taxable - income_tax - ni
  total_tax  <- income_tax + ni

  list(
    income_net = income_net,
    income_tax = income_tax,
    ni         = ni,
    total_tax  = total_tax
  )
}
