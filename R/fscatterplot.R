#' Draws interactive 2 or 3d scatterplots over data
#'
#' When plotting in three dimensions, the first dimension is on the x,
#' second is y, and third is z.
#'
#' Use the `event_source` parameter to link selection/brushing of the plot
#' to a listener.
#'
#' @rdname fscatterplot
#' @export
#'
#' @param x a data object
#' @param axes the definition of the x,y,z axes
#' @param webgl If `TRUE`, the plot will be rendered as WebGL, using plotly's
#'   [plotly::toWebGL()] function. Defaults to `FALSE` (svg).
#' @inheritParams maybe_facet
#' @return a plotly object
#' @examples
#' dat <- data.frame(
#'   a = rnorm(100), b = rnorm(100), c = rnorm(100),
#'   class = sample(c("g1", "g2", "g3"), 100, replace = TRUE),
#'   grp = sample(c("n", "o", "p", "q", "r"), 100, replace = TRUE))
#' dat <- head(dat, 10)
#' fscatterplot(dat, c("a", "b"), color_aes = "class", hover = "class")
#' fscatterplot(dat, c("a", "b"), color_aes = "class", hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b"), color_aes = "class", shape_aes = "grp",
#'              facet_aes = "class", hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b"), color_aes = "class", shape_aes = "grp",
#'              facet_aes = "grp", hover = c("class", "c"))
#'
#' # 3d
#' fscatterplot(dat, c("a", "b", "c"),color_aes = "class",
#'              hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b", "c"), color_aes = "class",
#'              hover = c("class", "c"), webgl = TRUE)
fscatterplot <- function(dat, axes,
                         color_aes = NULL, color_map = NULL,
                         shape_aes = NULL, shape_map = NULL,
                         size_aes = NULL, size_map = NULL,
                         facet_aes = NULL, facet_nrows = NULL,
                         hover = NULL, webgl = FALSE,
                         ...,
                         xlabel = NULL, ylabel = NULL, zlabel = NULL,
                         # direct plot_ly params:
                         marker_size = 8,
                         sizes = c(10, 100),
                         event_source = "A") {
  UseMethod("fscatterplot", dat)
}

#' @rdname fscatterplot
#' @method fscatterplot data.frame
#' @export
fscatterplot.data.frame <- function(dat, axes,
                                    color_aes = NULL, color_map = NULL,
                                    shape_aes = NULL, shape_map = NULL,
                                    size_aes = NULL, size_map = NULL,
                                    facet_aes = NULL, facet_nrows = NULL,
                                    hover = NULL, webgl = FALSE, ...,
                                    xlabel = NULL, ylabel = NULL, zlabel = NULL,
                                    marker_size = 8,
                                    sizes = c(10, 100),
                                    event_source = "A") {
  assert_character(axes, min.len = 2L, max.len = 3L)
  assert_subset(axes, names(dat))
  assert_subset(c(color_aes, shape_aes, size_aes, facet_aes, hover), names(dat))

  if (length(axes) == 3L && !is.null(facet_aes)) {
    stop("Can not facet a 3d plot")
  }

  xx <- with_aesthetics(dat, color_aes = color_aes, color_map = color_map,
                        shape_aes = shape_aes, shape_map = shape_map,
                        size_aes = size_aes, size_map = size_map,
                        hover = hover)

  xf <- paste0("~", axes[1])
  yf <- paste0("~", axes[2])
  zf <- paste0("~", axes[3])

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

  plot <- maybe_facet(.fscatterplot, xx, facet_aes, facet_nrows,
                      axes = axes, xf = xf, yf = yf, zf = zf,
                      marker_size = marker_size, .color = .color,
                      .colors = .colors, .shape = .shape,
                      .shapes = .shapes, ..., xlabel = xlabel,
                      ylabel = ylabel, zlabel = zlabel,
                      event_source = event_source)
  out <- list(plot = plot, input_data = dat, params = list())

  if (webgl) {
    out$plot <- toWebGL(out$plot)
  }
  class(out) <- c("FacileScatterViz", "FacileViz")
  out
}

#' The lowest-level scatterplot funciton that generates a plotly plot object.
#' The caller will decorate the result with all the appropriate class info
#' and such
#'
#' @noRd
.fscatterplot <- function(xx, axes, xf, yf, zf, facet_aes, facet_nrows,
                          marker_size, .color, .colors,
                          .shape, .shapes, ...,
                          xlabel, ylabel, zlabel, event_source) {
  xaxis <- list(title = if (!is.null(xlabel)) xlabel[1L] else axes[1L])
  yaxis <- list(title = if (!is.null(ylabel)) ylabel[1L] else axes[2L])
  if (length(axes) == 2L) {
    p <- plot_ly(xx, x = formula(xf), y = formula(yf),
                 type = "scatter",  mode = "markers",
                 marker = list(size = marker_size),
                 color = .color, colors = .colors,
                 symbol = .shape, symbols = .shapes,
                 text = ~.hover, source = event_source)
    p <- layout(p, xaxis = xaxis, yaxis = yaxis, dragmode = "select")
  } else {
    # camera settings from:
    # https://plot.ly/python/3d-camera-controls/
    # https://plot.ly/~klara.roehrl/225/camera-controls-eye-x01-y25-z01/#plot
    zaxis <- list(title = if (!is.null(zlabel)) zlabel[1L] else axes[3L])
    camera <- list(
      center = list(x = 0, y = 0, z = 0),
      up = list(x = 0, y = 0, z = 1),
      eye = list(x = 0.2, y = 2, z = 0.1))

    scene <- list(xaxis = xaxis, yaxis = zaxis, zaxis = yaxis, camera = camera)

    p <- plot_ly(xx,
                 x = formula(xf), y = formula(zf), z = formula(yf),
                 # x = formula(zf), y = formula(xf), z = formula(yf),
                 type = "scatter3d", mode = "markers",
                 color = .color, colors = .colors,
                 symbol = .shape, symbols = .shapes,
                 marker = list(size = marker_size),
                 text = ~.hover, source = event_source)
    p <- layout(p, scene = scene)
    p <- layout(p, dragmode = "select")
  }
  p
}

fscatterplot.default <- function(dat, axes,
                                 color_aes = NULL, color_map = NULL,
                                 shape_aes = NULL, shape_map = NULL,
                                 size_aes = NULL, size_map = NULL,
                                 facet_aes = NULL, facet_nrows = NULL,
                                 hover = NULL,
                                 ...,
                                 xlabel = NULL, ylabel = NULL, zlabel = NULL,
                                 # direct plot_ly params:
                                 marker_size = 8,
                                 sizes = c(10, 100),
                                 event_source = "A") {
  stop("This isn't real just yet, but it would look something like this")
  # Imagine x is a DGEList, ExpressionSet, SummarizedExperiment, etc.
  # This will delegate to the appropriate method in FacileBioc or elsewhere
  fscatterplot(facilitate(dat), axes,
               color_aes = color_aes, color_map = color_map,
               shape_aes = shape_aes, shape_map = shape_map,
               size_aes = size_aes, size_map = size_map,
               facet_aes = facet_aes, facet_nrows = facet_nrows,
               hover = hover, ..., marker_size = marker_size, sizes = sizes,
               event_source = event_source)
}

#' @rdname fscatterplot
#' @method fscatterplot tbl
#' @export
fscatterplot.tbl <- function(dat, axes,
                             color_aes = NULL, color_map = NULL,
                             shape_aes = NULL, shape_map = NULL,
                             size_aes = NULL, size_map = NULL,
                             facet_aes = NULL, facet_nrows = NULL,
                             hover = NULL, ...,
                             xlabel = NULL, ylabel = NULL, zlabel = NULL,
                             marker_size = 8,
                             sizes = c(10, 100),
                             event_source = "A") {
  fscatterplot.data.frame(collect(dat, Inf), axes,
                          color_aes = color_aes, color_map = color_map,
                          shape_aes = shape_aes, shape_map = shape_map,
                          size_aes = size_aes, size_map = size_map,
                          hover = hover, facet_aes = facet_aes,
                          facet_nrows = facet_nrows, ...,
                          xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
                          marker_size = marker_size,
                          sizes = sizes, event_source = event_source)
}

fscatterplot.FaclieDataStore <- function(dat, axes,
                                         color_aes = NULL, color_map = NULL,
                                         shape_aes = NULL, shape_map = NULL,
                                         size_aes = NULL, size_map = NULL,
                                         facet_aes = NULL, facet_nrows = NULL,
                                         hover = NULL,
                                         ...,
                                         xlabel = NULL, ylabel = NULL, zlabel = NULL,
                                         # direct plot_ly params:
                                         marker_size = 8,
                                         sizes = c(10, 100),
                                         event_source = "A") {

}

fscatterplot.FacileViz <- function(dat, axes,
                                   color_aes = NULL, color_map = NULL,
                                   shape_aes = NULL, shape_map = NULL,
                                   size_aes = NULL, size_map = NULL,
                                   facet_aes = NULL, facet_nrows = NULL,
                                   hover = NULL, ...,
                                   xlabel = NULL, ylabel = NULL, zlabel = NULL,
                                   marker_size = 8,
                                   sizes = c(10, 100),
                                   event_source = "A") {
  stop("This isn't real just yet, but it would look something like this")
  # delegate to fscatterpot.{tbl|data.frame}
  fscatterplot(tidy(dat), axes,
               color_aes = color_aes, color_map = color_map,
               shape_aes = shape_aes, shape_map = shape_map,
               size_aes = size_aes, size_map = size_map,
               facet_aes = facet_aes, facet_nrows = facet_nrows,
               hover = hover, ...,
               xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
               marker_size = marker_size, sizes = sizes,
               event_source = event_source)
}
