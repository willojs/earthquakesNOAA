#' eq_create_label
#'
#' This function contructs a vector of html-formatted strings by using the input \code{data}'s
#' \code{location}, \code{magnitude}, \code{total_deaths} columns. Those columns are titled
#' accordingly. If, for any of the rows, one of the values is missing, that particular part
#' of the annotation will be excluded.
#'
#' @param data A Dataframe Object containing the data to create the html annotations. Defaults to "LOCATION_NAME".
#' @param location Name (char) of the column in \code{data} depicting the location to be annotated. Defaults to "EQ_PRIMARY".
#' @param magnitude Name (char) of the column in \code{data} depicting the magnitude to be annotated. Defaults to "TOTAL_DEATHS".
#' @param total_deaths Name (char) of the column in \code{data} depicting the total_deaths to be annotated
#'
#' @return This function returns a character vector of HTML-formmated strings, labeling the \code{location},
#' \code{magnitude}, and \code{total_deaths} values.
#'
#' @examples
#'
#' \dontrun{
#'
#' readr::read_delim("NOAAearthquakes.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
#' }
#'
#' @export
#'

eq_create_label = function(data,
                           location = "LOCATION_NAME",
                           magnitude = "EQ_PRIMARY",
                           total_deaths = "TOTAL_DEATHS"){

  html_string_vector = c()
  for (i in 1:nrow(data)){
    html_string = ''
    if (!is.na(data[i, location])){
      html_string = paste(html_string, "<b>Location:</b>", data[i, location], "<br/>")
    }
    if (!is.na(data[i, magnitude])){
      html_string = paste(html_string, "<b>Magnitude:</b>", data[i, magnitude], "<br/>")
    }
    if (!is.na(data[i, total_deaths])){
      html_string = paste(html_string, "<b>Total deaths:</b>", data[i, total_deaths])
    }
    html_string_vector = c(html_string_vector, html_string)
  }
  html_string_vector
}
