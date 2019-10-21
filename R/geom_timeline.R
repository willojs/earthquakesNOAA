#' Timeline of earthquakes
#'
#' @description This geom plots a timeline of earthquakes in one line with
#' options to group by country, color by number of casualties and size by scale
#'
#' @inheritParams ggplot2::geom_point
#'
#' @details The function plots a timeline of earthquakes based on cleaned NOAA
#' data. It requires \code{x} aesthetics. An optional \code{y} aesthetics can
#' be used to group data by a selected variable (for example country).
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
#'    ggplot(aes(x = DATE,
#'               y = COUNTRY,
#'               color = as.numeric(TOTAL_DEATHS),
#'               size = as.numeric(EQ_PRIMARY)
#'    )) +
#'    geom_timeline() +
#'    theme_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths")
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob gList gpar
#' @importFrom scales alpha
GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                               shape = 21, fill = "grey", stroke = 0.5),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }

      coords <- coord$transform(data, panel_scales)

      points <- grid::pointsGrob(
        coords$x, coords$y,
        pch = coords$shape, size = unit(coords$size / 4, "char"),
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(coords$colour, coords$alpha)
        )
      )
      y_lines <- unique(coords$y)

      lines <- grid::polylineGrob(
        x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
        y = unit(c(y_lines, y_lines), "npc"),
        id = rep(seq_along(y_lines), 2),
        gp = grid::gpar(col = "grey",
                        lwd = .pt)
      )

      grid::gList(points, lines)
    }
  )
