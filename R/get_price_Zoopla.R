#' @title Get the price estimate of a UK house from Zoopla
#'
#' @description This function scrapes a Zoopla page and returns the associated estimated price of the property.
#' @param link_house The link to the relevant page for the property (rather than street) on Zoopla.
#' @keywords Zoopla
#' @export
#' @examples
#' \dontrun{
#' get_price_Zoopla(link_house = 'https://www.zoopla.co.uk/property/1-oak-road/reigate/rh2-0bp/19534314')
#' }

get_price_Zoopla <- function(link_house){
  
  # Add error checks
  
  data <- xml2::read_html(link_house) %>% 
    rvest::html_nodes(".pdp-estimate__price") %>%
    rvest::html_text() %>% 
    stringr::str_trim()
  
  value_house <-  stringr::word(data, 3)[1] %>% 
    stringr::str_sub(2, -2) %>% 
    as.numeric()
  
  return(value_house)
  
}