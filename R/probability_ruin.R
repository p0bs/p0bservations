#' @title Calculate the probability of retirement ruin
#'
#' @description This function uses the Milevsky-Robinson method to analyse the
#' probability of retirement ruin, by parsimoniously meshing investment risk and
#' return, mortality estimates and spending rates without resorting to opaque
#' Monte Carlo simulations. For further details, see: Milevsky, M. and C.
#' Robinson; "A Sustainable Spending Rate without Simulation"; Financial
#' Analysts Journal, Volume 61, Number 6. (2005). Please note that these are
#' approximations, so do not rely on them for financial returns or planning.
#' All arguments are vectorised and are recycled in the standard R fashion.
#' @param return_expected The expected real return of the entire pension
#'   portfolio.
#' @param return_sd The projected standard deviation of the returns of the
#'   entire pension portfolio.
#' @param life_remaining_expected The median projected remaining lifespan of
#'   the individual in question.
#' @param rate_spend The annual spending rate applied by the individual to
#'   their pension portfolio.
#' @keywords Retirement
#' @export
#' @examples
#' probability_ruin(
#'   return_expected = 0.07,
#'   return_sd = 0.2,
#'   life_remaining_expected = 28.1,
#'   rate_spend = 0.05
#' )
#'
#' # Vectorised: sweep over spending rates
#' probability_ruin(
#'   return_expected = 0.07,
#'   return_sd = 0.2,
#'   life_remaining_expected = 28.1,
#'   rate_spend = c(0.03, 0.04, 0.05)
#' )
#'
#' @importFrom stats pgamma

probability_ruin <- function(
    return_expected,
    return_sd,
    life_remaining_expected,
    rate_spend) {

  check_numeric(return_expected)
  check_numeric(return_sd)
  check_numeric(life_remaining_expected)
  check_numeric(rate_spend)

  check_positive(return_expected)
  check_positive(return_sd)
  check_positive(life_remaining_expected)
  check_positive(rate_spend)

  gp <- gamma_params(return_expected, return_sd, life_remaining_expected)
  stats::pgamma(q = rate_spend, shape = gp$shape, scale = gp$scale)
}
