utils::globalVariables(c("LOCATION_NAME","I_D","YEAR","MONTH","DAY","LATITUDE","LONGITUDE",
                         "EQ_PRIMARY","COUNTRY","STATE","TOTAL_DEATHS","DATE","YEAR4"))
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
#'
#' @return This function returns the clean version of the LOCATION_NAME
#'
#' @examples
#' \dontrun{
#' library(readr)
#' data <- readr::read_delim(("extdata","earthquake.txt", delim = "\t")
#' data <- eq_location_clean(data)
#' }
#'
#' @importFrom dplyr %>% mutate
#'
#' @export

eq_location_clean <- function(locationtoclean) {
  location_clean <- locationtoclean %>%
    dplyr::mutate(LOCATION_NAME=gsub("^.*:"," ",LOCATION_NAME)) %>%
    dplyr::mutate(LOCATION_NAME=gsub("\\b([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2" ,LOCATION_NAME, perl=TRUE))
  location_clean
}
