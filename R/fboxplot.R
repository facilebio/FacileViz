#' Create an interactive boxplot
#'
#' @param dat the data source
#' @param x the (categorical) covariate for the x axis
#' @param y the (real valued) values for the y
#' @param color_aes for the color of boxplot and points
#' @param shape_aes for the points
#' @param size_aes for the points
#' @param hover_aes for the points
#' @param group_aes group bar plots along the `x` categorical axis
#'
fboxplot <- function(dat, x, y,
                     color_aes = NULL, color_map = NULL,
                     shape_aes = NULL, shape_map = NULL,
                     size_aes = NULL, size_map = NULL,
                     hover_aes = NULL, hover_map = NULL,
                     group_aes = NULL,
                     hover = NULL,
                     ...,
                     xlabel = NULL, ylabel = NULL, zlabel = NULL,
                     # direct plot_ly params:
                     marker_size = 8,
                     sizes = c(10, 100),
                     event_source = "A") {
  UseMethod("fscatterplot", dat)
}
