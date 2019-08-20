## function: eq_location_clean()
##
##
#' Cleans the variable "LOCATION_NAME"
#'
#' \code{eq_location_clean} strips down from the LOCATION_NAME variable the
#' country. The function input is the whole earth quake data, of which it then
#' mutates the LOCATION_NAME.
#'
#' @param data A data frame of NOAA significant earthquake data
#'
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2_chr
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#' @importFrom dplyr mutate
#'
#' @return it returns the data frame back, but with the mutated LOCATION_NAME
#'
#' @export
#'
eq_location_clean <- function(data){
  data <- data %>%
    dplyr::mutate(
      LOCATION_NAME = purrr::map2_chr(COUNTRY, LOCATION_NAME,
                                      # stripping out the country name (including the colon)
                                      function(COUNTRY, LOCATION_NAME) {
                                        base::gsub(paste0(COUNTRY, ":"),
                                                   "",
                                                   LOCATION_NAME)
                                      }),
      # taking care of white spaces left:
      LOCATION_NAME = stringr::str_trim(LOCATION_NAME),

      # converts names to title case
      LOCATION_NAME = stringr::str_to_title(LOCATION_NAME)
    )

}
