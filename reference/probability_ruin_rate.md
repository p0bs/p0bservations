# Calculate the spend rate for a given probability of retirement ruin

This function uses the Milevsky-Robinson method to find the spend rate
corresponding to a given probability of retirement ruin, by
parsimoniously meshing investment risk and return, mortality estimates
and spending rates without resorting to opaque Monte Carlo simulations.
For further details, see: Milevsky, M. and C. Robinson; "A Sustainable
Spending Rate without Simulation"; Financial Analysts Journal, Volume
61, Number 6. (2005). Please note that these are approximations, so do
not rely on them for financial returns or planning. All arguments are
vectorised and are recycled in the standard R fashion.

## Usage

``` r
probability_ruin_rate(
  return_expected,
  return_sd,
  life_remaining_expected,
  value_probability_ruin
)
```

## Arguments

- return_expected:

  The expected real return of the entire pension portfolio.

- return_sd:

  The projected standard deviation of the returns of the entire pension
  portfolio.

- life_remaining_expected:

  The median projected remaining lifespan of the individual in question.

- value_probability_ruin:

  The desired probability of retirement ruin, strictly between 0 and 1.

## Examples

``` r
probability_ruin_rate(
  return_expected = 0.07,
  return_sd = 0.2,
  life_remaining_expected = 28.1,
  value_probability_ruin = 0.1
)
#> [1] 0.02961815

# Vectorised: sweep over target ruin probabilities
probability_ruin_rate(
  return_expected = 0.07,
  return_sd = 0.2,
  life_remaining_expected = 28.1,
  value_probability_ruin = c(0.05, 0.1, 0.2)
)
#> [1] 0.02144814 0.02961815 0.04230316
```
