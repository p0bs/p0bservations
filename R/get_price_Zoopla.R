#' @title Get the price estimate of a UK house from Zoopla
#'
#' @description This function scrapes a Zoopla page and returns the associated estimated price of the property.
#' @param link_house The link to the relevant page for the property (rather than street) on Zoopla.
#' @keywords Zoopla
#' @export
#' @examples
#' \dontrun{
#' get_price_Zoopla(link_house = 'https://www.zoopla.co.uk/property/1-oak-road/reigate/rh2-0bp/19534314')
#' get_price_Zoopla(link_house = 'https://www.zoopla.co.uk/property/10-waverleigh-road/cranleigh/gu6-8bz/9852441')
#' }

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
  
  detail <- xml2::read_html(link_house) %>% 
    rvest::html_nodes(".pdp-estimate__price") %>%
    rvest::html_text() %>% 
    stringr::str_split(., "\u00A3") %>% 
    .[[1]] %>% 
    .[2]
  
  detail_first <- stringr::str_sub(
    string = detail, 
    start = 1L,
    end = -2L) %>% 
    as.numeric()
  
  detail_last <- stringr::str_sub(
    string = detail, 
    start = -1L,
    end = -1L)
  
  value_out <- detail_first * dplyr::case_when(
    detail_last == "k" ~ 1000,
    detail_last == "m" ~ 1000000,
    TRUE ~ NA_real_
  )
  
  return(value_out)
  
}