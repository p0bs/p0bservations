#' @title Calculate the spend rate for a given probability of retirement ruin
#'
#' @description This function uses the Milevsky-Robinson method to find the
#' spend rate corresponding to a given probability of retirement ruin, by
#' parsimoniously meshing investment risk and return, mortality estimates and
#' spending rates without resorting to opaque Monte Carlo simulations. For
#' further details, see: Milevsky, M. and C. Robinson; "A Sustainable Spending
#' Rate without Simulation"; Financial Analysts Journal, Volume 61, Number 6.
#' (2005). Please note that these are approximations, so do not rely on them
#' for financial returns or planning. All arguments are vectorised and are
#' recycled in the standard R fashion.
#' @param return_expected The expected real return of the entire pension
#'   portfolio.
#' @param return_sd The projected standard deviation of the returns of the
#'   entire pension portfolio.
#' @param life_remaining_expected The median projected remaining lifespan of
#'   the individual in question.
#' @param value_probability_ruin The desired probability of retirement ruin,
#'   strictly between 0 and 1.
#' @keywords Retirement
#' @export
#' @examples
#' probability_ruin_rate(
#'   return_expected = 0.07,
#'   return_sd = 0.2,
#'   life_remaining_expected = 28.1,
#'   value_probability_ruin = 0.1
#' )
#'
#' # Vectorised: sweep over target ruin probabilities
#' probability_ruin_rate(
#'   return_expected = 0.07,
#'   return_sd = 0.2,
#'   life_remaining_expected = 28.1,
#'   value_probability_ruin = c(0.05, 0.1, 0.2)
#' )
#'
#' @importFrom stats qgamma

probability_ruin_rate <- function(
    return_expected,
    return_sd,
    life_remaining_expected,
    value_probability_ruin) {

  check_numeric(return_expected)
  check_numeric(return_sd)
  check_numeric(life_remaining_expected)
  check_numeric(value_probability_ruin)

  check_positive(return_expected)
  check_positive(return_sd)
  check_positive(life_remaining_expected)
  check_probability(value_probability_ruin)

  gp <- gamma_params(return_expected, return_sd, life_remaining_expected)
  stats::qgamma(p = value_probability_ruin, shape = gp$shape, scale = gp$scale)
}
