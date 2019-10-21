#' eq_location_clean
#' Parse LOCATION_NAME field of NOAA dataset
#'
#' @param data A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned LOCATION_NAME column
#'
#'
#' @param location_name represents the detailed information about location
#'
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace str_trim str_to_title
#'
#' @return This function returns the clean version of the LOCATION_NAME
#'
#' \dontrun{
#' data <- readr::read_delim("NOAAearthquakes.txt", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'
#' @export
#'

eq_location_clean <- function(data) {
  data <- data %>%
    dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                     stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                     stringr::str_trim("both") %>%
                     stringr::str_to_title())
  return(data)
}

