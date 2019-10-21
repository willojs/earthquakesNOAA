#' Timeline labels of earthquakes
#'
#' @description This geom plots timeline labels of earthquakes. It assumes that
#' \code{geom_timeline} was used to create the timelines
#'
#' @inheritParams ggplot2::geom_text

#' @param n_max An integer. If used, it only plots the labels for the
#' \code{n_max} largest earthquakes in the selected group in the timeline
#'
#' @details The function plots timeline labels of earthquakes based on cleaned
#' NOAA data. It should be used with combination with \code{geom_timeline}. The
#' required aesthetics for this geom is \code{label} that should contain
#' string for labeling each data point.
#'
#' @export
#'
#' @importFrom ggplot2 layer
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
#'    geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
#'    theme_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths")
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                n_max = NULL, show.legend = NA,
                                inherit.aes = TRUE) {

  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

#' @importFrom ggplot2 draw_key_blank
#' @importFrom dplyr %>% group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList
GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel", ggplot2::Geom,
    required_aes = c("x", "label"),
    draw_key = ggplot2::draw_key_blank,
    setup_data = function(data, params) {
      if (!is.null(params$n_max)) {
        if (!("size" %in% colnames(data))) {
          stop(paste("'size' aesthetics needs to be",
                     "provided when 'n_max' is defined."))
        }
        data <- data %>%
          dplyr::group_by_("group") %>%
          dplyr::top_n(params$n_max, size) %>%
          dplyr::ungroup()
      }
      data
    },
    draw_panel = function(data, panel_scales, coord, n_max) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }

      coords <- coord$transform(data, panel_scales)
      n_grp <- length(unique(data$group))
      offset <- 0.2 / n_grp

      lines <- grid::polylineGrob(
        x = unit(c(coords$x, coords$x), "npc"),
        y = unit(c(coords$y, coords$y + offset), "npc"),
        id = rep(1:dim(coords)[1], 2),
        gp = grid::gpar(
          col = "grey"
        )
      )

      names <- grid::textGrob(
        label = coords$label,
        x = unit(coords$x, "npc"),
        y = unit(coords$y + offset, "npc"),
        just = c("left", "bottom"),
        rot = 45
      )

      grid::gList(lines, names)
    }
  )
