#' Theme for better timeline visualization in ggplot2
#'
#' @description  This is a simple theme that makes \code{\link{geom_timeline}}
#' look better.
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#' filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
#'    ggplot(aes(x = DATE,
#'               y = COUNTRY,
#'               color = as.numeric(TOTAL_DEATHS),
#'               size = as.numeric(EQ_PRIMARY)
#'    )) +
#'    geom_timeline() +
#'    theme_timeline()
#' }
#'
#' @import ggplot2
#'
#' @export
#'
theme_timeline <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 1),
    axis.ticks.y = ggplot2::element_blank(),
    legend.position = "bottom"
  )
}
