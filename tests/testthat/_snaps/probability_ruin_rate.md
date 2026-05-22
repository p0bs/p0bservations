# error checks work

    Code
      probability_ruin_rate(return_expected = "high", return_sd = 0.1,
        life_remaining_expected = 28.1, value_probability_ruin = 0.1)
    Condition
      Error in `probability_ruin_rate()`:
      ! Input must be numeric.

---

    Code
      probability_ruin_rate(return_expected = -0.04, return_sd = 0.1,
        life_remaining_expected = 28.1, value_probability_ruin = 0.1)
    Condition
      Error in `probability_ruin_rate()`:
      ! All values must be positive.

---

    Code
      probability_ruin_rate(return_expected = 0.04, return_sd = 0.1,
        life_remaining_expected = 28.1, value_probability_ruin = 0)
    Condition
      Error in `probability_ruin_rate()`:
      ! Value must be a probability strictly between 0 and 1.

---

    Code
      probability_ruin_rate(return_expected = 0.04, return_sd = 0.1,
        life_remaining_expected = 28.1, value_probability_ruin = 1.2)
    Condition
      Error in `probability_ruin_rate()`:
      ! Value must be a probability strictly between 0 and 1.

