#' Create an interactive boxplot
#'
#' @export
#'
#' @param dat the data source
#' @param x the (categorical) covariate for the x axis
#' @param y the (real valued) values for the y
#' @param group_aes column in `dat` to group barplots by (not yet implemented)
#'
#' @examples
#' dat <- data.frame(
#'   variety = rep(LETTERS[1:7], each=40),
#'   treatment = rep(c("high","low"),each=20),
#'   note = seq(1:280)+sample(1:150, 280, replace=TRUE))
#'
#' # Dueling banjos
#'
#' ## Ungrouped
#' ggplot(dat, aes(x = variety, y = note)) +
#'   geom_boxplot()
#' fboxplot(dat, x = "variety", y = "note")
#'
#' ggplot(dat, aes(x = variety, y = note)) +
#'   geom_boxplot() +
#'   facet_wrap(~ treatment)
#' fboxplot(dat, x = "variety", y = "note", facet_aes = "treatment")
#'
#' ## Grouped
#' ggplot(dat, aes(x=variety, y=note, fill=treatment)) +
#'   geom_boxplot()
#' fboxplot(dat, x = "variety", y = "note", group_aes = "treatment")
fboxplot <- function(dat, x, y, with_points = FALSE, group_aes = NULL,
                     color_aes = NULL, color_map = NULL,
                     shape_aes = NULL, shape_map = NULL,
                     size_aes = NULL, size_map = NULL,
                     facet_aes = NULL, facet_nrows = NULL,
                     hover = NULL,
                     ...,
                     xlabel = NULL, ylabel = NULL,
                     # direct plot_ly params:
                     marker_size = 8,
                     sizes = c(10, 100),
                     pointpos = -1.8,
                     event_source = "A") {
  UseMethod("fboxplot", dat)
}


#' @export
#' @rdname fboxplot
fboxplot.data.frame <- function(dat, x, y, with_points = nrow(dat) < 1000,
                                group_aes = NULL,
                                color_aes = NULL, color_map = NULL,
                                shape_aes = NULL, shape_map = NULL,
                                size_aes = NULL, size_map = NULL,
                                facet_aes = NULL, facet_nrows = NULL,
                                hover = NULL,
                                ...,
                                xlabel = NULL, ylabel = NULL,
                                # direct plot_ly params:
                                marker_size = 8,
                                sizes = c(10, 100),
                                pointpos = -1.8,
                                event_source = "A") {
  assert_string(x)
  assert_string(y)
  assert_subset(c(x, y), names(dat))

  xx <- with_aesthetics(dat, color_aes = color_aes, color_map = color_map,
                        shape_aes = shape_aes, shape_map = shape_map,
                        size_aes = size_aes, size_map = size_map,
                        hover = hover, ...)

  .colors <- suppressWarnings(aes_map(xx, "color"))
  if (is.null(.colors)) {
    .color <- I("black")
  } else {
    .color.columns <- attr(.colors, "columns")
    .color <- formula(paste0("~", .color.columns[["variable"]]))
  }

  .shapes <- suppressWarnings(aes_map(xx, "shape"))
  if (is.null(.shapes)) {
    .shape <- I("circle")
  } else {
    .shape.columns <- attr(.shapes, "columns")
    .shape <- formula(paste0("~", .shape.columns[["variable"]]))
  }

  plot <- maybe_facet(.fboxplot, xx, facet_aes, facet_nrows,
                      x, y, with_points, group_aes,
                      marker_size = marker_size, .color = .color,
                      .colors = .colors, .shape = .shape, .shapes = .shapes,
                      ..., xlabel = xlabel, ylabel = ylabel,
                      pointpos = pointpos,
                      event_source = event_source)
  plot
}

#' The lowest-level scatterplot funciton that generates a plotly plot object.
#' The caller will decorate the result with all the appropriate class info
#' and such
#'
#' @noRd
.fboxplot <- function(xx, x, y, with_points, group_aes, facet_aes, facet_nrows,
                      marker_size, .color, .colors, .shape, .shapes, ...,
                      xlabel, ylabel, pointpos, event_source) {
  xaxis <- list(title = x)
  yaxis <- list(title = y)

  xf <- paste0("~", x)
  yf <- paste0("~", y)

  boxpoints <- if (with_points) "all" else "suspectedoutliers"

  p <- plot_ly(xx, x = formula(xf), y = formula(yf), text = ~.hover) %>%
    add_boxplot(color = .color, colors = .colors,
                boxpoints = boxpoints, pointpos = pointpos) %>%
    layout(xaxis = xaxis, yaxis = yaxis, dragmode = "select")
  p
}

fboxplot.tbl <- function(dat, x, y, with_points = FALSE, group_aes = NULL,
                         color_aes = NULL, color_map = NULL,
                         shape_aes = NULL, shape_map = NULL,
                         size_aes = NULL, size_map = NULL,
                         facet_aes = NULL, facet_nrows = NULL,
                         hover = NULL,
                         ...,
                         xlabel = NULL, ylabel = NULL,
                         # direct plot_ly params:
                         marker_size = 8,
                         sizes = c(10, 100),
                         pointpos = -1.8,
                         event_source = "A") {
  fboxplot.data.frame(collect(dat, n = Inf),
                      x = x, y = y, with_points = with_points,
                      group_aes = group_aes,
                      color_aes = color_aes, color_map = color_map,
                      shape_aes = shape_aes, shape_map = shape_map,
                      size_aes = size_aes, size_map = size_map,
                      facet_aes = facet_aes, facet_nrows = facet_nrows,
                      hover = hover,
                      ...,
                      xlabel = xlabel, ylabel = ylabel,
                      # direct plot_ly params:
                      marker_size = marker_size,
                      sizes = sizes,
                      pointpos = pointpos,
                      event_source = event_source)
}

if (FALSE) {
  # library
  library(ggplot2)

  # create a data frame
  variety=rep(LETTERS[1:7], each=40)
  treatment=rep(c("high","low"),each=20)
  note=seq(1:280)+sample(1:150, 280, replace=T)


  data=data.frame(variety, treatment ,  note)


  # grouped boxplot
  ggplot(data, aes(x=variety, y=note, fill=treatment)) +
    geom_boxplot()

  fboxplot(data, x = "variety", y = "note",
           group = "treatment")
  fboxplot(data, x = "variety", y = "note",
           group = "treatment",
           with_points = TRUE)

  # facet
  ggplot(data, aes(x=variety, y=note, fill=treatment)) +
    geom_boxplot() +
    facet_wrap(~variety)

  fboxplot(data, x = "variety", y = "note",
           group_aes = "treatment",
           facet_aes = "variety")

  ggplot(data, aes(x=variety, y=note, fill=treatment)) +
    geom_boxplot() +
    facet_wrap(~treatment)

  fboxplot(data, x = "variety", y = "note",
           group_aes = "treatment",
           facet_aes = "treatment")

}
