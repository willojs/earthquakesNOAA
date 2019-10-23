#' More interesting pop-ups for the interactive map used with the eq_map() function
#'
#' @param mapdata A cleaned data frame with data obtained from NOAA website
#'
#' @return An HTML label that can be used as the annotation text in the leaflet map.
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquake.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#'
#' @export

eq_create_label <- function(mapdata){
  paste(ifelse(is.na(mapdata$LOCATION_NAME),"", paste("<b>Location: </b>",mapdata$LOCATION_NAME,"<br/>")),
        ifelse(is.na(mapdata$EQ_PRIMARY),"", paste("<b>Magnitude: </b>",mapdata$EQ_PRIMARY,"<br/>")),
        ifelse(is.na(mapdata$TOTAL_DEATHS),"", paste("<b>Total deaths: </b>",mapdata$TOTAL_DEATHS,"<br/>")))
}
