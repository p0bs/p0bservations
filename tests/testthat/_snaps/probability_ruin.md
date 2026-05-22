# error checks work

    Code
      probability_ruin(return_expected = -0.05, return_sd = 0.1,
        life_remaining_expected = 22, rate_spend = 0.05)
    Condition
      Error in `probability_ruin()`:
      ! All values must be positive.

---

    Code
      probability_ruin(return_expected = 0.05, return_sd = 0.1,
        life_remaining_expected = "thirty years", rate_spend = 0.05)
    Condition
      Error in `probability_ruin()`:
      ! Input must be numeric.

