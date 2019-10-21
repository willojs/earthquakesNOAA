#' data Earthquake NOAA Data
#'
#' function: eq_clean_data()
#'
#'
#' \code{eq_clean_data()} that takes raw NOAA data frame and returns a clean data frame.
#' The clean data frame should have the following:
#' A date column created by uniting the year, month, day and converting it to the \code{Date} class.
#' \code{LATITUDE} and \code{LONGITUDE} columns converted to numeric class.
#'
#'
#' @param raw contains the original raw dataset
#'
#' @return This function returns the clean version of the dataset
#'
#' @import dplyr
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom lubridate ymd
#' @importFrom stringr str_pad
#'
#' @examples
#'
#' \dontrun{
#' data <- readr::read_delim("NOAAearthquakes.txt", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'

#'
#' @export
eq_clean_data <- function(data) {
  data <- data %>%
    dplyr::mutate_(
      year_fix = ~stringr::str_pad(as.character(abs(YEAR)), width = 4,
                                   side = "left", pad = "0"),
      date_paste = ~paste(year_fix, MONTH, DAY, sep = "-"),
      DATE = ~lubridate::ymd(date_paste, truncated = 2)) %>%
    dplyr::select_(quote(-year_fix), quote(-date_paste))

  lubridate::year(data$DATE) <- data$YEAR

  data <- data %>%
    dplyr::mutate_(LATITUDE = ~as.numeric(LATITUDE),
                   LONGITUDE = ~as.numeric(LONGITUDE))

  data <- eq_location_clean(data)

  data
}
