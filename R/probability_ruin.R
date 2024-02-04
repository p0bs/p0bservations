#' @title Calculate the probability of retirement ruin
#'
#' @description This function uses the Milevsky-Robinson to analyse the probability of retirement ruin, by parsimoniously meshing investment risk and return, mortality estimates and spending rates without resorting to opaque Monte Carlo simulations. For further details, see: Milevsky, M. and C. Robinson; "A Sustainable Spending Rate without Simulation"; Financial Analysts Journal, Volume 61, Number 6. (2005)
#' @param return_expected The expected real return of the entire pension portfolio
#' @param return_sd The projected standard deviation of the returns of the entire pension portfolio
#' @param life_remaining_expected The median projected remaining lifespan of the individual in question
#' @param rate_spend The annual spending rate applied by the individual to their pension portfolio
#' @keywords Retirement
#' @export
#' @examples
#' \dontrun{
#' probability_ruin(
#' return_expected = 0.035, 
#' return_sd = 0.1, 
#' life_remaining_expected = 30, 
#' rate_spend = 0.03
#' )
#' }
#' 

probability_ruin <- function(return_expected, return_sd, life_remaining_expected, rate_spend){
  
  value_lambda <- log(2) / life_remaining_expected
  value_scale = ((return_sd * return_sd) + value_lambda) / 2
  value_shape = ((return_expected + (2 * value_lambda)) / value_scale) - 1
  
  stats::pgamma(q = rate_spend, shape = value_shape, scale = value_scale)
}