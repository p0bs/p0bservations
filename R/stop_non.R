#' @name stop_not_scalar_double
#' @title Stop the function if zero or more than one value is entered 
#' @description Error function to stop the function if zero or more than one value is entered 
#' @param value_entered The value to be error-checked by the function
#' @keywords errors
#' @examples
#' \dontrun{
#' stop_not_scalar(value_entered = age)
#' }
#' 
NULL

#' @name stop_not_positive
#' @title Stop the function the value entered is (or is below) zero 
#' @description Error function to stop the function if the value entered is (or is below) zero 
#' @param value_entered The value to be error-checked by the function
#' @keywords errors
#' @examples
#' \dontrun{
#' stop_not_positive(value_entered = age)
#' }
#' 
NULL

#' @rdname stop_not_scalar_double
stop_not_scalar_double <- function(value_entered){
  if (!rlang::is_scalar_double(value_entered)){ 
    rlang::abort("You must enter a numeric value (and only one numeric value)") 
  } 
}

#' @rdname stop_not_positive
stop_not_positive <- function(value_entered){
  if (value_entered <= 0){ 
    rlang::abort("value entered must be positive") 
  }
}