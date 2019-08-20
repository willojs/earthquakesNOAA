#' function: eq_clean_data()
#'
#'
#' This function returns the raw NOAA data frame with a new \code{DATE} column (of class date) added
#' by combining the MONTH, YEAR, and DAY fields (MONTHS and DAYS with NA are replaced to 01).
#' Furthermore, the rows with NA YEAR, or YEAR < 0 are removed. The LATITUDE and LONGITUDE fields
#' are converted to numeric type, and the LOCATION_NAME fixing (from the eq_location_clean function)
#' is implemented.
#'
#'
#' @param data Dataset depicting the raw NOAA data frame.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#'
#' @return it returns the data frame, cleaned and with correct dates (with date class)
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' dataset <- read_delim("NOAAearthquakes.txt", delim = "\t")
#' dataset <- eq_clean_data(dataset)
#'
#' }
#' @export
#'

  eq_clean_data <- function(data){

    ## Adding PlaceHolder Days and Months to Missing Fields
    data[is.na(data$MONTH), "MONTH"] = 1
    data[is.na(data$DAY), "DAY"] = 1

    ## Removing Years that are negative
    data = data %>%
      dplyr::filter('YEAR > 0')

    ## Creating Date Columns from individual YEAR, MONTH, DAY columns
    data$DATE = as.Date(paste(data$YEAR, data$MONTH, data$DAY, sep = "-"), format = "%Y-%m-%d")

    ## Changing the LONGITUDE and LATITUDE types to numeric
    data$LATITUDE = as.numeric(data$LATITUDE)
    data$LONGITUDE = as.numeric(data$LONGITUDE)

    # Changing Deaths to numeric (For ease of display)
    data$DEATHS = as.numeric(data$DEATHS)
    data$TOTAL_DEATHS = as.numeric(data$TOTAL_DEATHS)

    ## Fixing Location Names
    data <- eq_location_clean(data)

    return(data)
  }

