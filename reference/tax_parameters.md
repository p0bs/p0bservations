# Key income tax parameters for the UK.

A dataset containing the main tax parameters used for calculating net
income for the UK (outside of Scotland).

## Usage

``` r
tax_parameters
```

## Format

A data frame with 3 rows (one for each tax year) and 14 variables:

- year_tax_end:

  the calendar year in which the end of the tax year occurs

- level_ni_lower:

  the lower level breakpoint for National Insurance

- level_ni_upper:

  the upper level breakpoint for National Insurance

- level_allowance_lower:

  the lower level breakpoint for personal allowance

- level_allowance_upper:

  the upper level breakpoint for personal allowance

- level_tax_higher:

  the income breakpoint for higher rate income tax

- level_tax_upper:

  the income breakpoint for upper rate income tax

- rate_ni_lower:

  the lower level tax rate for National Insurance

- rate_ni_upper:

  the upper level tax rate for National Insurance

- rate_allowance_drop:

  the rate at which personal allowance drops after
  \`level_allowance_upper\`

- rate_tax_basic:

  the basic rate of income tax

- rate_tax_higher:

  the higher rate of income tax

- rate_tax_upper:

  the upper rate of income tax

- rate_tax_sales:

  the rate of sales tax, also called VAT

## Source

<https://www.crunch.co.uk/knowledge-tax/tax-rates-thresholds-and-allowances-for-current-tax-year/>
