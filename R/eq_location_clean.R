#' eq_location_clean
#'
#' This function modifies the LOCATION_NAME column of a dataframe by separating the LOCATION_NAME
#' into multiple strings (separated by the character ':'), and selecting the last one. This step
#' transforms the LOCATION_NAME field from states and cities (towns) to just cities.
#' It also correctly formats the LOCATION_NAME so that it only has its first letter capitalized.
#'
#' @param data Dataset depicting the raw NOAA data frame.
#'
#' @importFrom stringr str_replace_all
#' @importFrom tools toTitleCase
#' @importFrom dplyr mutate
#' @importFrom utils tail
#' @importFrom dplyr %>%
#'
#' @return This function returns the raw NOAA data frame with the LOCATION_NAME columns cleaned up.
#'
#' @examples
#'
#' \dontrun{
#'
#' dataset <- read_delim("NOAAearthquakes.txt", delim = "\t")
#' dataset <- eq_location_clean(dataset)
#'
#' }
#'
#' @export
#'

eq_location_clean <- function(data){
  data = data %>%
    ## Extracting Last Word (City/Town) from Location name
    dplyr::mutate(LOCATION_NAME = 'sapply(strsplit(LOCATION_NAME, ":"), utils::tail, 1)') %>%
    ## Removing the Starting and Ending Blank Spaces from the new Location Names
    dplyr::mutate(LOCATION_NAME = 'trimws(LOCATION_NAME)') %>%
    ## Titleing the Locations properly.
    dplyr::mutate(LOCATION_NAME = 'tools::toTitleCase(tolower(LOCATION_NAME))')

  return(data)
}
