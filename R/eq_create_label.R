#' Creates a label for leaflet map
#'
#' This function creates a label for the \code{leaflet} map based on location
#' name, magnitude and casualties from NOAA earthquake data
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#'
#' @return A character vector with labels
#'
#' @details The input \code{data.frame} needs to include columns LOCATION_NAME,
#' EQ_PRIMARY and TOTAL_DEATHS with the earthquake location, magintude and
#' total casualties respectively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
eq_create_label <- function(data) {
  popup_text <- with(data, {
    part1 <- ifelse(is.na(LOCATION_NAME), "",
                    paste("<strong>Location:</strong>", LOCATION_NAME))
    part2 <- ifelse(is.na(EQ_PRIMARY), "",
                    paste("<br><strong>Magnitude</strong>", EQ_PRIMARY))
    part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                    paste("<br><strong>Total deaths:</strong>", TOTAL_DEATHS))
    paste0(part1, part2, part3)
  })
}
