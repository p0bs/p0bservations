# Get the latest annuity rates for the UK

This function retrieves data on UK annuity rates from Sharing Pensions.
Rates are available for different specifications and ages. See the
website for further details. Please note that these are approximations,
so do not rely on them for financial returns or planning.

## Usage

``` r
rate_annuity(value_age, value_specifications)
```

## Source

\<https://www.sharingpensions.co.uk/annuity_rates.htm\>

## Arguments

- value_age:

  These are the choices for the age of the annuitant, namely: \`55\`,
  \`60\`, \`65\`, \`70\` or \`75\`. The ensuing annuity rates imply that
  the annuity begins to be paid immediately and thereafter on a monthly
  basis.

- value_specifications:

  These are the specifications available, varying by: whether the
  annuity is on a single basis or a joint basis (and, if so, whether the
  last remaining spouse receives half or all of the annuity); whether
  the annuity is guaranteed for ten years or not; and whether the
  payments will escalate at 3

  single_simple

  :   single, level rate, no guarantee

  single_guaranteed

  :   single, level rate, guaranteed

  single_escalating

  :   single, escalating rate, no guarantee

  half_simple

  :   joint, half upon first death, level rate, no guarantee

  full_simple

  :   joint, all upon first death, level rate, no guarantee

  half_escalating

  :   joint, half upon first death, escalating rate, no guarantee

## Examples

``` r
if (FALSE) { # \dontrun{
rate_annuity(value_age, value_specifications)
} # }
```
