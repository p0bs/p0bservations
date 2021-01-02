#' @title A Cool Function
#'
#' @description This function allows you to express your love for the superior team.
#' @param agree Do you agree that Chelsea is the best team? Defaults to TRUE.
#' @keywords Chelsea
#' @export
#' @examples
#' test()

test <- function(agree=TRUE){
  if(agree==TRUE){
    print("Carefree!")
  }
  else {
    print("Try again.")
  }
}