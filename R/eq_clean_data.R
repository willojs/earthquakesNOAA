#' Clean the raw NOAA earquake data
#'
#' @param datatoclean A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned date, latitude and longitude numerical columns
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquake.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' }
#'
#' @importFrom dplyr %>% mutate select if_else
#' @importFrom tidyr unite
#' @importFrom lubridate year ymd
#' @importFrom lubridate DATE
#'
#' @export

eq_clean_data <- function(cleandata) {
  clean_data <- cleandata %>%
    dplyr::select(I_D, YEAR, MONTH, DAY, LATITUDE, LONGITUDE, LOCATION_NAME, EQ_PRIMARY, COUNTRY, STATE, TOTAL_DEATHS) %>%
    dplyr::mutate(YEAR4=sprintf("%04d",as.numeric(gsub('-','',YEAR)))) %>%
    dplyr::mutate(MONTH=dplyr::if_else(is.na(MONTH),'01',sprintf("%02d", MONTH))) %>%
    dplyr::mutate(DAY=dplyr::if_else(is.na(DAY),'01',sprintf("%02d", DAY))) %>%
    tidyr::unite(DATE,YEAR4,MONTH,DAY,sep='-',remove = FALSE) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    dplyr::select(-YEAR4)

  lubridate::year(clean_data$DATE) <- clean_data$YEAR

  clean_data <- clean_data %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),LONGITUDE = as.numeric(LONGITUDE),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY), TOTAL_DEATHS = as.numeric(TOTAL_DEATHS))
  clean_data
}
