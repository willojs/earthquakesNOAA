#' Visualize the times at which earthquakes occur within certain countries
#'
#' @return The geom \code{geom_timeline} used with the \code{ggplot} function,is used to display time series events ranging from one
#' time point to another time point
#'
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created.
#' @param data  The data to be displayed in this layer.
#' @param stat  The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#'
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @export
#'
#' @examples
#' # The data must be cleaned using the function \code{eq_clean_data}, included in the package.
#' # Aesthetics can be specified in the \code{ggplot} function or in \code{geom_timeline} geom function
#' \dontrun{
#' data <- readr::read_delim("earthquake.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' data %>%
#' dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
#' ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#' geom_timeline(alpha=.5) +
#' theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#' ggtitle("Earthquakes Visualization Tool") +
#' labs(size = "Richter scale value", color = "# deaths")
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' GeomTimeline
#'
#' GeomTimeline Geom coding
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid segmentsGrob gpar pointsGrob gList
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = "x",
                                 default_aes = ggplot2::aes(y=0, colour="black", shape=19, size=1, stroke = 0.5, alpha = 0.5, fill = NA),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   grid::gList(
                                     grid::pointsGrob(
                                       coords$x, coords$y,
                                       pch = coords$shape,
                                       gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                       lwd = coords$stroke * .stroke / 2)
                                     ),
                                     grid::segmentsGrob(
                                       x0 = min(coords$x), y0 = coords$y, x1 = max(coords$x), y1 = coords$y,
                                       gp = grid::gpar(col = "black", lwd = 1)
                                     )
                                   )
                                 }
)
