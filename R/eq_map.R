#' eq_map
#'
#' This function takes a \code{data} argument that must include LATITUDE and LONGITUDE columns, and displays
#' its data points in a leaflet map. The points are depicted as circles, with popup annotations being displayed
#' upon clicking. These annotations depend on the \code{annot_col} argument.
#'
#' @param data A Dataframe Object containing the data to plot (Must include a LATITUDE and LONGITUDE colums).
#' @param annot_col Column of `data` depicting the markers that will be used in the map (upon clicking).
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr %>%
#'
#' @return This function adds the map as well as the datapoints, positioned at their respective Lat and Long
#' coordinates, into the current graphics device. Furthermore, it add popup markers for each datapoint depending
#' on the \code{annot_col} arguments.
#'
#' @examples
#'
#' \dontrun{
#'
#' readr::read_delim("NOAAearthquakes.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#'
#' }
#'
#' @export
#'

eq_map = function(data, annot_col){
  annotation = data[[annot_col]]
  mapping = leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              popup = ~ annotation,
                              weight = 1)
  return(mapping)
}
