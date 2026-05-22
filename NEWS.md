# p0bservations 0.4.0

-   `liability_tax()` now accepts a numeric vector for `income_taxable`; all output list elements are vectors of the same length. The `rowwise()` implementation has been replaced with vectorised `pmin()`/`pmax()` operations.
-   `probability_ruin()` now accepts numeric vectors for all parameters, which are recycled in the standard R fashion.
-   `probability_ruin_rate()` now uses `stats::qgamma()` — the exact analytical inverse of the gamma CDF — replacing a numerical binary search. The function now accepts numeric vectors for all parameters. The previous implementation was bounded to spend rates below 5%; this restriction is removed.
-   Input validation across all functions now uses classed conditions (e.g. `"p0bservations_error_not_numeric"`) so callers can catch specific error types. The `tax_year_end` argument in `liability_tax()` now correctly accepts integer literals (e.g. `2024L`).
-   The `readr` package dependency has been removed.

# p0bservations 0.3.3

-   Add GitHub Action for wasm-related needs

# p0bservations 0.3.2

-   Add `probability_ruin_rate` to iterate over `probability_ruin` so as to find the spend rate that generates the desired probability of retirement ruin

# p0bservations 0.3.1

-   Tweaking `rate_annuity` error checks

# p0bservations 0.3.0

-   Updating tax calculations to contain better error checks and generate many different outputs
-   Adding `probability_ruin` calculation
-   Adding `rate_annuity` to pull UK annuity rates from the internet

# p0bservations 0.2.1

-   Updating tax calculations in `calc_income_net` for latest tax year
-   Removing `get_price_Zoopla`
-   Updating GitHub Actions to `v2`

# p0bservations 0.2.0

-   Enabling tax calculations in different years for `calc_income_net`

# p0bservations 0.1.3

-   Making `get_price_Zoopla` more robust by altering the scraping procedure, using a newer version of `rvest`
-   Adding empty line to end of `_pkgdown.yml`

# p0bservations 0.1.2

-   Simplify `get_price_Zoopla` to reflect new Zoopla page layout. Remove Notes from CRAN check

# p0bservations 0.1.1

-   Added `calc_income_net` to perform a simple tax & NI calculation for a UK resident.

# p0bservations 0.1.0

-   Added a `NEWS.md` file to track changes to the package.
-   Added `get_price_Zoopla` to retrieve house price estimates for British properties.
-   Adding links to github and pkgdown sites
