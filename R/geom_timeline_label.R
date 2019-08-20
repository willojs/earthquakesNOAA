#' GeomTimelineLabel
#'
#' This Geom returns a ggproto object made of a gList of lineGrobs and textGrobs. Each lineGrob is a vertical line located
#' at the points' x-axis. The height of the lines depends on how many y-degrees are given.
#' The Text grobs writes the text found on the \code{label} argument. There will be one label per row, although the rows are filtered
#' for only the top \code{nmax} labels, based on their \code{size} column. The text is rotated 45 degrees, and it's positioned slightly
#' above the top end of the lineGrob.
#' The Geom allows you to label the higher-valued (based on \code{size}) points in your dataset.
#'
#' @section Parameters:
#'
#' \code{data}: A Dataframe Object containing the data to plot. \cr \cr
#' \code{x}: Column of `data` depicting the x-coordinate of where each of the labels will be located. \cr \cr
#' \code{label}: Column of `data` depicting the text labels that will be displayed. \cr \cr
#' \code{size}: Column of `data` depicting size of the Earthquake, which will be used to decide the top \code{nmax} labels displayed. \cr \cr
#' \code{y}: OPTIONAL. Column of `data` depicting the y-coordinate of each label displayed. \cr \cr
#' \code{nmax}: OPTIONAL. Column of 'data' depicting how many top \code{size}d labels will be displayed. \cr \cr
#' \code{alpha}: OPTIONAL. Alpha numeric value (0 to 1, added outside of aes) which will be used to apply the alpha to the vertical line. \cr \cr
#' \code{colour}: OPTIONAL. Color 'string' (added outside of aes) to modify the default color of the lines. \cr \cr
#' \code{stroke}: OPTIONAL. Stroke of the image (need not be used). \cr \cr
#'
#' *Note: All of the optional arguments above can be applied as columns of the dataset (inside the function aes),
#' or as individual values (outside of the function aes).
#'
#' @importFrom scales alpha
#' @importFrom grid unit
#' @importFrom grid gList
#' @importFrom grid linesGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_abline
#' @importFrom ggplot2 Geom
#' @importFrom dplyr arrange
#' @importFrom utils head
#'
#'
#' @return This function return a Geom* object containing the line and text Grobs responsible for rendering the images into the plot.
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
#'   geom_timeline_label(ggplot2::aes(label = dataset$LOCATION_NAME)) +
#'   ggplot2::guides(size = ggplot2::guide_legend(title = "Ritcher scale value")) +
#'   ggplot2::scale_colour_continuous(name = "# of DEATHS") +
#'   ggplot2::theme_classic()
#'
#' }
#'
#' @export
#'

GeomTimelineLabel = ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                     ## For the labeling to work, we need both the labels,
                                     ## their location, and their size to use as filter
                                     required_aes = c("x", "label", "size"),
                                     default_aes = ggplot2::aes(y = 0.5, nmax = 5, stroke = 0,
                                                                colour = NA, alpha = 0.5),
                                     draw_key = ggplot2::draw_key_abline,
                                     setup_data = function(data, params){

                                       ## We calculate the amount of distinct groups (y-axis)
                                       line_length = length(unique(data$y))
                                       if (line_length < 1){line_length = 1;}

                                       ## Return the variable as part of the data
                                       data$line_length = line_length
                                       data
                                     },

                                     ## For each group (y-axis)
                                     draw_group = function(data, panel_scales, coord){

                                       ## Transforming parameters to panel coordinates
                                       coords <- coord$transform(data, panel_scales)

                                       ## Creating The List of Grobs to draw plot
                                       main_list = grid::gList() # List of grobs to be displayed

                                       ## Make data into dataframe to extract top [nmax] labels (based on size)
                                       data_point = data.frame(x = coords$x, y = coords$y,
                                                               size = coords$size, label = coords$label)

                                       data_point = data_point %>%
                                         dplyr::arrange(desc(size)) %>%
                                         utils::head(coords$nmax[1])

                                       ## For each of the top labels
                                       for (i in 1:nrow(data_point)){
                                         ## Creating Geom Line
                                         lG = grid::linesGrob(
                                           rep(data_point[i, "x"], 2), # Making it a vertical line
                                           c(data_point[i, "y"],
                                             data_point[i, "y"] + 0.15/coords$line_length), # Adjusting the height based on degrees of y
                                           gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha/2))
                                         )

                                         ## Creating Geom Text
                                         tG = grid::textGrob(
                                           data_point[i, "label"], # text to write
                                           data_point[i, "x"],
                                           data_point[i, "y"] + 0.18/coords$line_length, # position slightly above line
                                           rot = 45, # Degrees to rotate by
                                           just = c("left", "bottom"), # Corner to center the text on
                                           gp = grid::gpar(col="grey20", fontsize=12, fontface = "plain") # text aethetics
                                         )

                                         ## Add both Grobs to the main Grob List
                                         main_list = grid::gList(main_list, lG, tG)
                                       }

                                       main_list
                                     }
)


#' geom_timeline_label
#'
#'
#' This layer function displays the labels of Earthquakes (coming from your \code{label}) by
#' presenting each label, rotated 45 degrees, as text.
#' The top \code{nmax} labels, based on the \code{size} column, will be displayed on top of a vertical line.
#' This vertical line has one end located at the Earthquakes' x-coordinates (dates), and the other end under the label.
#' If given a \code{y} argument, the vertical lines will be displayed in two groups at each of the y-coordinates; the
#' top \code{nmax} labels will be displayed for EACH y-coordinate.
#'
#' @section Parameters to Geom:
#'
#' \code{data}: A Dataframe Object containing the data to plot. \cr \cr
#' \code{x}: Column of `data` depicting the x-coordinate of where each of the labels will be located. \cr \cr
#' \code{label}: Column of `data` depicting the text labels that will be displayed. \cr \cr
#' \code{size}: Column of `data` depicting size of the Earthquake, which will be used to decide the top \code{nmax} labels displayed. \cr \cr
#' \code{y}: OPTIONAL. Column of `data` depicting the y-coordinate of each label displayed. \cr \cr
#' \code{nmax}: OPTIONAL. Column of 'data' depicting how many top \code{size}d labels will be displayed. \cr \cr
#' \code{alpha}: OPTIONAL. Alpha numeric value (0 to 1, added outside of aes) which will be used to apply the alpha to the vertical line. \cr \cr
#' \code{colour}: OPTIONAL. Color 'string' (added outside of aes) to modify the default color of the lines. \cr \cr
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
#' @importFrom ggplot2 layer
#'
#' @return This function adds the lineGrobs and textGrobs into the current graphics device.
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
#'   geom_timeline_label(ggplot2::aes(label = dataset$LOCATION_NAME)) +
#'   ggplot2::guides(size = ggplot2::guide_legend(title = "Ritcher scale value")) +
#'   ggplot2::scale_colour_continuous(name = "# of DEATHS") +
#'   ggplot2::theme_classic()
#'
#' }
#'
#' @export
#'

geom_timeline_label = function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
