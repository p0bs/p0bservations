# error checks work

    Code
      liability_tax(income_taxable = "15000", tax_year_end = 2024L)
    Condition
      Error in `liability_tax()`:
      ! Input must be numeric.

---

    Code
      liability_tax(income_taxable = -20000, tax_year_end = 2024)
    Condition
      Error in `liability_tax()`:
      ! All values must be positive.

---

    Code
      liability_tax(income_taxable = 38000, tax_year_end = c(2024, 2025))
    Condition
      Error in `liability_tax()`:
      ! Value must be a single integer.

