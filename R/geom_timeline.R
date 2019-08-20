#' GeomTimeline
#'
#' This Geom returns a ggproto object made of a gList of lineGrobs and pointGrobs. Each lineGrob is a straight horizontal line ranging
#' from the minimum x-value to maximum x-value. This horizontal line is done for each unique y-value.
#' The Point grobs are colored based on the \code{colour} argument. There will be one point per row.
#'
#'
#' @section Parameters to Geom:
#'
#' \code{data}: A Dataframe Object containing the data to plot. \cr \cr
#' \code{x}: Column of `data` depicting the x-coordinate of where each of the points will be located. \cr \cr
#' \code{size}: OPTIONAL. Column of `data` depicting size of each point in the pointsGrob displayed (if
#' missing, the first \code{nmax} labels will be displayed). \cr \cr
#' \code{y}: OPTIONAL. Column of `data` depicting the y-coordinate of where each of the points will be located. \cr \cr
#' \code{colour}: OPTIONAL. Column of `data` depicting the colour of each point in the pointGrob displayed. \cr \cr
#' \code{shape}: OPTIONAL. Column of 'data' depicting the shape of each of the points. \cr \cr
#' \code{alpha}: OPTIONAL. Column of `data` which will be used to apply the alpha to each point. \cr \cr
#' \code{fill}: OPTIONAL. Color 'string' (added outside of aes) to modify the default fill of the points (before applying \code{colour}). \cr \cr
#' \code{stroke}: OPTIONAL. Stroke of the image (need not be used). \cr \cr
#'
#' *Note: All of the optional arguments above can be applied as columns of the dataset (inside the function aes),
#' or as individual values (outside of the function aes).
#'
#'
#' @importFrom scales alpha
#' @importFrom grid unit
#' @importFrom grid gList
#' @importFrom grid linesGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 Geom
#'
#'
#' @return This function return a Geom* object containing the line and point Grobs responsible for rendering the images into the plot.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Data Setup
#'
#' dataset = readr::read_delim("NOAAearthquakes.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
#'
#' ## Function Call
#'
#' ggplot2::ggplot(dataset, ggplot2::aes(DATE,
#'                                       colour = TOTAL_DEATHS,
#'                                       y = COUNTRY,
#'                                       size = as.numeric(EQ_PRIMARY) )) +
#'   geom_timeline() +
#'   ggplot2::guides(size = ggplot2::guide_legend(title = "Ritcher scale value")) +
#'   ggplot2::scale_colour_continuous(name = "# of DEATHS") +
#'   ggplot2::theme_classic()
#'
#' }
#'
#' @export
#'

GeomTimeline = ggplot2::ggproto("GeomTimeline",
                                ggplot2::Geom,
                                required_aes = "x",
                                default_aes = ggplot2::aes(shape = 21, y = 0.5,
                                                           fill = "grey10", alpha = 0.5,
                                                           colour = "black", size = 6,
                                                           stroke = 0),
                                draw_key = ggplot2::draw_key_point,

                                draw_panel = function(data, panel_scales, coord){

                                  ## Transforming parameters to panel coordinates
                                  coords <- coord$transform(data, panel_scales)

                                  ## Creating The List of Grobs to draw plot
                                  main_list = grid::gList() # List of grobs to be displayed

                                  ## Creating Straight Line in the entire x-range
                                  line_range = c(min(coords$x), max(coords$x))

                                  for (elem in unique(coords$y)){ # For each y value (Country)
                                    ## Draw straight line (minx to maxx) at each y level
                                    lG = grid::linesGrob(
                                      line_range,
                                      elem,
                                      gp = grid::gpar(alpha = coords$alpha/2)
                                    )
                                    ## Add Grob to the main List
                                    main_list = grid::gList(main_list, lG)
                                  }

                                  ## Creating Points Geom to describe each data point (x-axis)
                                  coords$size = coords$size / 2.0 # Modify the size of points to a more aesthetic size
                                  pG = grid::pointsGrob(
                                    coords$x,
                                    coords$y,
                                    pch = coords$shape, # Shape of the points
                                    size = grid::unit(coords$size, "char"), # Size of each dot
                                    gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha),
                                                    fill = scales::alpha(coords$colour, coords$alpha),
                                                    alpha = coords$alpha,
                                                    fontsize = coords$size * ggplot2::.pt)
                                  )

                                  ## Add Points Grob to the main list
                                  grid::gList(pG, main_list)
                                }
)


#' geom_timeline
#'
#' This layer function displays the dates of Earthquakes (coming from your \code{data}) by
#' presenting each Earthquake, on a straight line, as a point of varying sizes depending on
#' their Ritcher scale value (based on the \code{size} variable). The Earthquakes are color
#' coded by the death toll it caused (\code{colour}). If the \code{y} variable is supplied,
#' the Earthquakes will be displayed in multiple horizontal lines, each line represnting
#' the quakes for each Country.
#'
#' @section Parameters to Geom:
#'
#' \code{data}: A Dataframe Object containing the data to plot. \cr \cr
#' \code{x}: Column of `data` depicting the x-coordinate of where each of the points will be located. \cr \cr
#' \code{size}: OPTIONAL. Column of `data` depicting size of each point in the pointsGrob displayed (if
#' missing, the first \code{nmax} labels will be displayed). \cr \cr
#' \code{y}: OPTIONAL. Column of `data` depicting the y-coordinate of where each of the points will be located. \cr \cr
#' \code{colour}: OPTIONAL. Column of `data` depicting the colour of each point in the pointGrob displayed. \cr \cr
#' \code{shape}: OPTIONAL. Column of 'data' depicting the shape of each of the points. \cr \cr
#' \code{alpha}: OPTIONAL. Column of `data` which will be used to apply the alpha to each point. \cr \cr
#' \code{fill}: OPTIONAL. Color 'string' (added outside of aes) to modify the default fill of the points (before applying \code{colour}). \cr \cr
#' \code{stroke}: OPTIONAL. Stroke of the image (need not be used). \cr \cr
#'
#' *Note: All of the optional arguments above can be applied as columns of the dataset (inside the function aes),
#' or as individual values (outside of the function aes).
#'
#'
#' @param data A Dataframe Object containing the data to plot.
#' @param mapping Mapping argument to the ggplot layer function.
#' @param stat Stat argument to the ggplot layer function.
#' @param position Position argument to the ggplot layer function.
#' @param na.rm na.rm argument to the ggplot layer function.
#' @param show.legend show.legend argument to the ggplot layer function.
#' @param inherit.aes inherit.aes argument to the ggplot layer function.
#' @param ... Extra Params.
#'
#' *Note: All of the optional arguments above can be applied as columns of the dataset (inside the function aes),
#' or as individual values (outside of the function aes).
#'
#' @importFrom ggplot2 layer
#'
#' @return This function adds the lineGrobs and pointGrob into the current graphics device.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Data Setup
#'
#' dataset = readr::read_delim("NOAAearthquakes.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000)
#'
#' ## Function Call
#'
#' ggplot2::ggplot(dataset, ggplot2::aes(DATE,
#'                                       colour = TOTAL_DEATHS,
#'                                       y = COUNTRY,
#'                                       size = as.numeric(EQ_PRIMARY) )) +
#'   geom_timeline() +
#'   ggplot2::guides(size = ggplot2::guide_legend(title = "Ritcher scale value")) +
#'   ggplot2::scale_colour_continuous(name = "# of DEATHS") +
#'   ggplot2::theme_classic()
#'
#' }
#'
#' @export
#'

geom_timeline = function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
