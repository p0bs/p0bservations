# error checks work

    Code
      rate_annuity(value_age = 55, value_specifications = "single_simple")
    Condition
      Error in `rate_annuity()`:
      ! `value_age` must be a character vector, not the number 55.

---

    Code
      rate_annuity(value_age = "55", value_specifications = "single")
    Condition
      Error in `rate_annuity()`:
      ! `value_specifications` must be one of "single_simple", "single_guaranteed", "single_escalating", "half_simple", "full_simple", or "half_escalating", not "single".

---

    Code
      rate_annuity(value_age = "55")
    Condition
      Error in `rate_annuity()`:
      ! `value_specifications` must be a character vector, not absent.

---

    Code
      rate_annuity(value_specifications = "single_simple")
    Condition
      Error in `rate_annuity()`:
      ! `value_age` must be a character vector, not absent.

