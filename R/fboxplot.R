#' Create an interactive boxplot
#'
#' @export
#'
#' @param dat the data source
#' @param x the (categorical) covariate for the x axis
#' @param y the (real valued) values for the y
#' @param group_aes column in `dat` to group barplots by (not yet implemented)
fboxplot <- function(dat, x, y, with_points = FALSE, group_aes = NULL,
                     color_aes = NULL, color_map = NULL,
                     shape_aes = NULL, shape_map = NULL,
                     size_aes = NULL, size_map = NULL,
                     hover = NULL,
                     ...,
                     xlabel = NULL, ylabel = NULL,
                     # direct plot_ly params:
                     marker_size = 8,
                     sizes = c(10, 100),
                     event_source = "A") {
  UseMethod("fboxplot", dat)
}


#' @export
#' @rdname fboxplot
fboxplot.data.frame <- function(dat, x, y, with_points = FALSE,
                                group_aes = NULL,
                                color_aes = NULL, color_map = NULL,
                                shape_aes = NULL, shape_map = NULL,
                                size_aes = NULL, size_map = NULL,
                                hover = NULL,
                                ...,
                                xlabel = NULL, ylabel = NULL,
                                # direct plot_ly params:
                                marker_size = 8,
                                sizes = c(10, 100),
                                event_source = "A") {
  assert_string(x)
  assert_string(y)
  if (!is.null(group_aes)) assert_string(group_aes)
  if (!is.null(color_aes)) assert_string(color_aes)
  if (!is.null(shape_aes)) assert_string(shape_aes)
  if (!is.null(size_aes)) assert_string(size_aes)

  assert_subset(c(x, y, group_aes, color_aes, shape_aes, size_aes),
                names(dat))

  xx <- dat
  xx <- with_color(xx, color_aes, aes_map = color_map)
  xx <- with_shape(xx, shape_aes, aes_map = shape_map)
  # xx <- with_size(xx, size_aes)
  # xx <- with_hover(xx, hover_aes)

  if (is.character(hover)) {
    hvals <- lapply(hover, function(wut) {
      vals <- xx[[wut]]
      if (is.numeric(vals)) vals <- prettyNum(round(vals, 2), big.mark = ",")
      if (!is.character(vals)) vals <- as.character(vals)
      paste0(wut, ": ", vals)
    })
    xx[[".hover"]] <- do.call(paste, c(hvals, list(sep = "<br>")))
  } else {
    xx[[".hover"]] <- ""
  }

}

#' The lowest-level scatterplot funciton that generates a plotly plot object.
#' The caller will decorate the result with all the appropriate class info
#' and such
#'
#' @noRd
.fboxplot <- function(xx, x, y, group_aes, ...) {

}

fboxplot.tbl <- function(dat, x, y, with_points = FALSE, group_aes = NULL,
                         color_aes = NULL, color_map = NULL,
                         shape_aes = NULL, shape_map = NULL,
                         size_aes = NULL, size_map = NULL,
                         hover = NULL,
                         ...,
                         xlabel = NULL, ylabel = NULL,
                         # direct plot_ly params:
                         marker_size = 8,
                         sizes = c(10, 100),
                         event_source = "A") {
  fboxplot.data.frame(collect(dat, n = Inf),
                      x = x, y = y, with_points = with_points,
                      group_aes = group_aes,
                      color_aes = color_aes, color_map = color_map,
                      shape_aes = shape_aes, shape_map = shape_map,
                      size_aes = size_aes, size_map = size_map,
                      hover = hover,
                      ...,
                      xlabel = xlabel, ylabel = ylabel,
                      # direct plot_ly params:
                      marker_size = marker_size,
                      sizes = sizes,
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
