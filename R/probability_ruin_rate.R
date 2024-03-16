#' @title Calculate the spend rate for a given probability of retirement ruin
#'
#' @description This function uses the Milevsky-Robinson to analyse the spend rate for a given probability of retirement ruin, by parsimoniously meshing investment risk and return, mortality estimates and spending rates without resorting to opaque Monte Carlo simulations. For further details, see: Milevsky, M. and C. Robinson; "A Sustainable Spending Rate without Simulation"; Financial Analysts Journal, Volume 61, Number 6. (2005). Please note that these are approximations, so do not rely on them for financial returns or planning.
#' @param return_expected The expected real return of the entire pension portfolio
#' @param return_sd The projected standard deviation of the returns of the entire pension portfolio
#' @param life_remaining_expected The median projected remaining lifespan of the individual in question
#' @param value_probability_ruin The desired probability of retirement ruin (which is used to solve for the corresponding spend rate)
#' @keywords Retirement
#' @export
#' @examples
#' probability_ruin_rate(
#'   return_expected = 0.07, 
#'   return_sd = 0.2, 
#'   life_remaining_expected = 28.1, 
#'   value_probability_ruin = 0.1
#'   )
#' 

probability_ruin_rate <- function(return_expected, return_sd, life_remaining_expected, value_probability_ruin){
  
  # Error checks ----
  stop_not_scalar_double(return_expected)
  stop_not_scalar_double(return_sd)
  stop_not_scalar_double(life_remaining_expected)
  stop_not_scalar_double(value_probability_ruin)
  
  stop_not_positive(return_expected)
  stop_not_positive(return_sd)
  stop_not_positive(life_remaining_expected)
  stop_not_positive(value_probability_ruin)
  
  
  # Main function ----
  
  rate_high <- 0.05
  rate_low <- 0.00001
  
  prob_high <- probability_ruin(
    return_expected = return_expected, 
    return_sd = return_sd, 
    life_remaining_expected = life_remaining_expected, 
    rate_spend = rate_high
  )
  
  prob_low <- probability_ruin(
    return_expected = return_expected, 
    return_sd = return_sd, 
    life_remaining_expected = life_remaining_expected, 
    rate_spend = rate_low
  )
  
  diff_high <- abs(value_probability_ruin - prob_high)
  diff_low <- abs(value_probability_ruin - prob_low)
  diff_max <- max(diff_low, diff_high)
  count <- 0
  
  while (diff_max > 0.001) {  
    
    if (diff_high < diff_low) {
      
      rate_low <- rate_low + ((rate_high - rate_low) / 3)
      
      prob_low <- probability_ruin(
        return_expected = return_expected, 
        return_sd = return_sd, 
        life_remaining_expected = life_remaining_expected, 
        rate_spend = rate_low
      )
      
      diff_low <- abs(value_probability_ruin - prob_low)
      
    } else {
      
      rate_high <- rate_high - ((rate_high - rate_low) / 3)
      
      prob_high <- probability_ruin(
        return_expected = return_expected, 
        return_sd = return_sd, 
        life_remaining_expected = life_remaining_expected, 
        rate_spend = rate_high
      )
      
      diff_high <- abs(value_probability_ruin - prob_high)
      
    }
    
    diff_max <- max(diff_low, diff_high)
    count <- count + 1
    
  }
  
  return((rate_high + rate_low) / 2)
}