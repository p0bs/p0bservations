#' @noRd
check_numeric <- function(x) {
  if (!is.numeric(x)) {
    rlang::abort(
      "Input must be numeric.",
      class = "p0bservations_error_not_numeric",
      call = rlang::caller_env()
    )
  }
}

#' @noRd
check_positive <- function(x) {
  if (any(x <= 0)) {
    rlang::abort(
      "All values must be positive.",
      class = "p0bservations_error_not_positive",
      call = rlang::caller_env()
    )
  }
}

#' @noRd
check_probability <- function(x) {
  if (any(x <= 0) || any(x >= 1)) {
    rlang::abort(
      "Value must be a probability strictly between 0 and 1.",
      class = "p0bservations_error_not_probability",
      call = rlang::caller_env()
    )
  }
}

#' @noRd
check_scalar_integerish <- function(x) {
  if (!rlang::is_scalar_integerish(x) || is.na(x)) {
    rlang::abort(
      "Value must be a single integer.",
      class = "p0bservations_error_not_scalar",
      call = rlang::caller_env()
    )
  }
}
