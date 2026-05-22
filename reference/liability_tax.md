# Calculate UK income and/or National Insurance taxes

This function applies the prevailing tax bands and rates to give a
simple calculation for UK Income Tax and National Insurance (to the
nearest couple of pounds). Please note that these are approximations, so
do not rely on them for financial returns or planning. The output is a
named list containing: \`income_net\` for the net income after Income
Tax and National Insurance; \`income_tax\` for the Income Tax liability;
\`ni\` for the National Insurance liability; and \`total_tax\` for the
combined liability. \`income_taxable\` may be a numeric vector; all
output list elements will be vectors of the same length.

## Usage

``` r
liability_tax(income_taxable, tax_year_end)
```

## Arguments

- income_taxable:

  The taxable income level (i.e. after deductions for things like
  pension contributions). May be a numeric vector.

- tax_year_end:

  The calendar year in which the tax year ends, as a YYYY integer. For
  example, tax year 2023/24 would be 2024.

## Examples

``` r
liability_tax(
  income_taxable = 38000,
  tax_year_end = 2024
)$total_tax
#> [1] 8137.6

# Vectorised over multiple incomes
liability_tax(
  income_taxable = c(22000, 40000, 65000),
  tax_year_end = 2024
)$total_tax
#> [1]  3017.6  8777.6 18250.6
```
