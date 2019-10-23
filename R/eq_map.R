#' Mapping the earthquake epicenters and providing some annotations from NOAA earthquake data
#'
#' This interactive map can show earquake information on popup
#'  accoring to given column
#'
#' @import leaflet
#' @param dat an imput dataframe
#' @param annot_col a string representing colume
#'
#' @return A map of the earthquakes epicenters and providing some annotations
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquake.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @export

eq_map <- function(mapdata, annot_col = "DATE") {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = mapdata$LONGITUDE, lat = mapdata$LATITUDE,
                              radius = as.numeric(mapdata$EQ_PRIMARY), popup = mapdata[[annot_col]],
                              stroke = FALSE, fillOpacity = 0.5)
}
