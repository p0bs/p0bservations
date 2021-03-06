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
  
  string <- xml2::read_html(link_house) %>% 
    rvest::html_nodes("p.css-1tz04i5-Text-StyledEstimatedPriceText.eb1eagn1") %>%
    rvest::html_text() 
  
  detail <- stringr::str_remove_all(
    stringr::str_split(
      string, 
      "\u00A3"
      )[[1]][2],
    "[:punct:]"
    )
  
  return(as.numeric(detail))
  
}