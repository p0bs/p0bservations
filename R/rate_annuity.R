#' @title Get the latest annuity rates for the UK
#'
#' @description This function retrieves data on UK annuity rates from Sharing Pensions. Rates are available for different specifications and ages. See the website for further details. Please note that these are approximations, so do not rely on them for financial returns or planning.
#' @param value_age These are the choices for the age of the annuitant, namely: `55`, `60`, `65`, `70` or `75`. The ensuing annuity rates imply that the annuity begins to be paid immediately and thereafter on a monthly basis.
#' @param value_specifications These are the specifications available, varying by: whether the annuity is on a single basis or a joint basis (and, if so, whether the last remaining spouse receives half or all of the annuity); whether the annuity is guaranteed for ten years or not; and whether the payments will escalate at 3% pa or not. In all, the following combinations are available: 
#' \describe{
#'   \item{single_simple}{single, level rate, no guarantee}
#'   \item{single_guaranteed}{single, level rate, guaranteed}
#'   \item{single_escalating}{single, escalating rate, no guarantee}
#'   \item{half_simple}{joint, half upon first death, level rate, no guarantee}
#'   \item{full_simple}{joint, all upon first death, level rate, no guarantee}
#'   \item{half_escalating}{joint, half upon first death, escalating rate, no guarantee}
#' }
#' @source <https://www.sharingpensions.co.uk/annuity_rates.htm>
#' @keywords Retirement
#' @export
#' @examples
#' \dontrun{
#' rate_annuity(value_age, value_specifications)
#' }
#' 
#' @importFrom rlang .data

rate_annuity <- function(value_age, value_specifications){
  
  # Error check ----
  values_age <- c("55", "60", "65", "70", "75")
  values_specifications <- c("single_simple", "single_guaranteed", "single_escalating", "half_simple", "full_simple", "half_escalating")
  
  if (!rlang::arg_match(arg = value_age, values = values_age, multiple = FALSE)){ 
    rlang::abort("You must enter one (and only one) age (as a character) from the list of possible values") 
  }
  
  if (!rlang::arg_match(arg = value_specifications, values = values_specifications, multiple = FALSE)){ 
    rlang::abort("You must enter one (and only one) specification (as a character) from the list of possible values") 
  }
  
  # Main code ----
  link_annuities <- "https://www.sharingpensions.co.uk/annuity_rates.htm"
  
  table_annuities_raw <- rvest::read_html(link_annuities) |> 
    rvest::html_elements("table") |> 
    _[30] |> 
    rvest::html_table()
  
  table_annuities_single <- table_annuities_raw[[1]] |> 
    dplyr::slice(5:9) |> 
    dplyr::select("age" = .data$X1, "single_simple" = .data$X2, "single_guaranteed" = .data$X4, "single_escalating" = .data$X6) |> 
    tidyr::pivot_longer(
      cols = -.data$age, 
      names_to = "specifications",
      values_to = "values", 
      values_transform = readr::parse_number
    ) 
  
  table_annuities_joint <- table_annuities_raw[[1]] |> 
    dplyr::slice(15:19) |> 
    dplyr::select("age" = .data$X1, "half_simple" = .data$X2, "full_simple" = .data$X4, "half_escalating" = .data$X6) |> 
    tidyr::pivot_longer(
      cols = -.data$age, 
      names_to = "specifications",
      values_to = "values", 
      values_transform = readr::parse_number
    )
  
  table_annuities <- dplyr::bind_rows(table_annuities_single, table_annuities_joint) |> 
    dplyr::mutate(rate = .data$values/100000) |> 
    dplyr::select(-.data$values)
  
  value_output <- table_annuities |>
    dplyr::filter(
      .data$age == value_age,
      .data$specifications == value_specifications
    ) |>
    dplyr::pull(.data$rate)
 
  return(value_output)
  
}
  