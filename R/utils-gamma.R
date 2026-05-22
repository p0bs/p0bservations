#' @noRd
gamma_params <- function(return_expected, return_sd, life_remaining_expected) {
  value_lambda <- log(2) / life_remaining_expected
  value_scale  <- (return_sd^2 + value_lambda) / 2
  value_shape  <- ((return_expected + 2 * value_lambda) / value_scale) - 1
  list(shape = value_shape, scale = value_scale)
}
