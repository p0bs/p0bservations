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
