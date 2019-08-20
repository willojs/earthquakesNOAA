## function: eq_clean_data()
##
##
# After downloading and reading in the dataset, the overall task for this module is
# to write a function named eq_clean_data()that takes raw NOAA data frame and returns
# a clean data frame. The clean data frame should have the following:

# The clean data frame should have the following:

# 1.  A date column created by uniting the year, month, day and converting it to the Date class
# 2.  LATITUDE and LONGITUDE columns converted to numeric class
# 3.  In addition, write a function eq_location_clean() that cleans the LOCATION_NAME column by
#     stripping out the country name (including the colon) and converts names to title case (as opposed
#     to all caps). This will be needed later for annotating visualizations. This function should be applied
#     to the raw data to produce a cleaned up version of the LOCATION_NAME column.

#' Loads and Cleans the Earth Quake data
#'
#' \code{eq_clean_data} loads the data, cleans the data (including the LOCATION_NAME variable)
#' and gives back the data frame. There is no input to this function
#'
#' @importFrom magrittr "%>%"
#' @importFrom readr read_tsv
#' @importFrom dplyr mutate
#'
#' @return it returns the data frame, cleaned and with correct dates (with date class)
#'
#' @export
eq_clean_data <- function() {

  data("NOAAearthquakesRAW")
  data <- NOAAearthquakesRAW %>%
    # data <- readr::read_tsv("./inst/extdata/NOAAearthquakes") %>%

    # replacing NA's in MONTH
    dplyr::mutate(MONTH = base::replace(MONTH, base::which(base::is.na(MONTH)), 1)) %>%

    # replacing NA's in DAY
    dplyr::mutate(DAY = base::replace(DAY, base::which(base::is.na(DAY)), 1)) %>%
    dplyr::mutate(date = base::ifelse(YEAR < 0,
                                      handle_negative_dates(YEAR, MONTH, DAY),
                                      handle_positive_dates(YEAR, MONTH, DAY)
    )) %>%
    dplyr::mutate(date = base::as.Date(date, origin = "1970-01-01")) %>%
    dplyr::mutate(LATITUDE  = base::as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = base::as.numeric(LONGITUDE))


  # clean location via eq_location_clean
  data <- eq_location_clean(data)

}
