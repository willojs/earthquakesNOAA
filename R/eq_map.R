#' Leaflet map of earthquakes
#'
#' This function creates a \code{leaflet} map of selected earthquakes based on
#' input NOAA earthquake cleaned data.
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#' @param annot_col A character. The name of the column in the data that should
#' be used as descriptor.
#'
#' @return A leaflet map with earthquakes and annotations.
#' @export
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_map(data, annot_col = "LOCATION_NAME")
#' }
eq_map <- function(data, annot_col) {

  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                              radius = data$EQ_PRIMARY, weight = 1,
                              popup = data[[annot_col]])

  m
}
