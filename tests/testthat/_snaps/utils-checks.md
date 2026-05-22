# check_numeric errors on non-numeric input

    Code
      check_numeric("text")
    Condition
      Error:
      ! Input must be numeric.

---

    Code
      check_numeric(TRUE)
    Condition
      Error:
      ! Input must be numeric.

# check_positive errors on zero or negative values

    Code
      check_positive(0)
    Condition
      Error:
      ! All values must be positive.

---

    Code
      check_positive(-1)
    Condition
      Error:
      ! All values must be positive.

---

    Code
      check_positive(c(1, -2, 3))
    Condition
      Error:
      ! All values must be positive.

# check_probability errors on out-of-range values

    Code
      check_probability(0)
    Condition
      Error:
      ! Value must be a probability strictly between 0 and 1.

---

    Code
      check_probability(1)
    Condition
      Error:
      ! Value must be a probability strictly between 0 and 1.

---

    Code
      check_probability(1.5)
    Condition
      Error:
      ! Value must be a probability strictly between 0 and 1.

---

    Code
      check_probability(-0.1)
    Condition
      Error:
      ! Value must be a probability strictly between 0 and 1.

# check_scalar_integerish errors on non-scalar or non-integer input

    Code
      check_scalar_integerish(c(1L, 2L))
    Condition
      Error:
      ! Value must be a single integer.

---

    Code
      check_scalar_integerish("2024")
    Condition
      Error:
      ! Value must be a single integer.

---

    Code
      check_scalar_integerish(NA_integer_)
    Condition
      Error:
      ! Value must be a single integer.

