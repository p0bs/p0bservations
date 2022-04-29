#' @title Get the price estimate of a UK house from Zoopla
#'
#' @description This function scrapes a Zoopla page and returns the associated estimated price of the property.
#' @param link_house The link to the relevant page for the property (rather than street) on Zoopla.
#' @keywords Zoopla
#' @export
#' @examples
#' \dontrun{
#' get_price_Zoopla(
#'   link_house = 'https://www.zoopla.co.uk/property/1-oak-road/reigate/rh2-0bp/19534314'
#'   )
#' get_price_Zoopla(
#'   link_house = 'https://www.zoopla.co.uk/property/10-waverleigh-road/cranleigh/gu6-8bz/9852441'
#'   )
#' }
#' 
#' @importFrom rlang .data

get_price_Zoopla <- function(link_house){
  
  if (!is.character(link_house)){ 
    rlang::abort("link_house must be a character input") 
  } 
  
  if (length(link_house) != 1){ 
    rlang::abort("You must enter one, and only one, entry for link_house") 
  } 
  
  if (stringr::str_sub(link_house, start = 1L, end = 34L) != "https://www.zoopla.co.uk/property/"){
    rlang::abort("link_house must begin with 'https://www.zoopla.co.uk/property/'")
  }
  
  # The paragraph for 'Estimated price' immediately precedes the estimated price itself
  
  string <- rvest::read_html(link_house) |> 
    rvest::html_elements("p") |> 
    rvest::html_text2() |> 
    tibble::as_tibble() |>  
    tibble::rownames_to_column()
  
  location <- string |> 
    dplyr::filter(
      stringr::str_detect(
        string = .data$value, 
        pattern = "Estimated price")
      ) |> 
    dplyr::slice(1) |> 
    dplyr::pull(.data$rowname) |>  
    as.integer()
  
  detail0 <- string |> 
    dplyr::slice(location + 1) |> 
    dplyr::pull(.data$value) |> 
    stringr::str_split(
      pattern = "\u00A3", 
      n = 2
      ) |> 
    unlist()
  
  detail <- detail0[2] |> 
    stringr::str_remove_all(pattern = "[:punct:]") |> 
    as.numeric()
  
  return(detail)
  
}