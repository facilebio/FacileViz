#' Draws interactive 2 or 3d scatterplots over data
#'
#' When plotting in three dimensions, the first dimension is on the x,
#' second is y, and third is z.
#'
#' Use the `event_source` parameter to link selection/brushing of the plot
#' to a listener.
#'
#' TODO: Get inspired from this marginal density plot and add that functionality
#' here via the `with_density = TRUE` parameter. When this is TRUE, faceting
#' should be axed.
#' https://plotly-r.com/arranging-views.html#fig:joint
#'
#' @rdname fscatterplot
#' @export
#'
#' @importFrom FacileData test_categorical
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
#' fscatterplot(dat, c("a", "b"), color_aes = "class",
#'              facet_aes = "class", hover = c("class", "c"),
#'              facet_nrows = 2)
#' fscatterplot(dat, c("a", "b"), color_aes = "class", shape_aes = "grp",
#'              facet_aes = "grp", hover = c("class", "c"))
#'
#' # 3d
#' fscatterplot(dat, c("a", "b", "c"),color_aes = "class",
#'              hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b", "c"), flat = TRUE, color_aes = "class",
#'              hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b", "c"), color_aes = "class", shape_aes = "grp",
#'              hover = c("class", "c"), webgl = TRUE)
fscatterplot <- function(dat, axes, with_density = FALSE,
                         color_aes = NULL, color_map = NULL,
                         shape_aes = NULL, shape_map = NULL,
                         size_aes = NULL, size_map = NULL,
                         facet_aes = NULL, facet_nrows = NULL,
                         hover = NULL, webgl = FALSE, showlegend = TRUE,
                         legendside = c("bottom", "right", "none"),
                         ...,
                         flat = FALSE,
                         xlabel = NULL, ylabel = NULL, zlabel = NULL,
                         # direct plot_ly params:
                         marker_size = 8,
                         height = 600,
                         width = height + (height / 6),
                         sizes = c(10, 100),
                         event_source = "A") {
  UseMethod("fscatterplot", dat)
}

#' @rdname fscatterplot
#' @method fscatterplot data.frame
#' @importFrom plotly toWebGL
#' @export
fscatterplot.data.frame <- function(dat, axes, with_density = FALSE,
                                    color_aes = NULL, color_map = NULL,
                                    shape_aes = NULL, shape_map = NULL,
                                    size_aes = NULL, size_map = NULL,
                                    facet_aes = NULL, facet_nrows = NULL,
                                    hover = NULL, webgl = FALSE,
                                    showlegend = TRUE,
                                    legendside = c("bottom", "right", "none"),
                                    ...,
                                    flat = FALSE,
                                    xlabel = NULL, ylabel = NULL, zlabel = NULL,
                                    marker_size = 8,
                                    height = 600,
                                    width = height + (height / 6),
                                    sizes = c(10, 100),
                                    key = NULL, event_source = "A") {
  assert_character(axes, min.len = 2L, max.len = 3L)
  assert_subset(axes, names(dat))
  assert_subset(c(color_aes, shape_aes, size_aes, facet_aes, hover), names(dat))
  assert_flag(with_density)
  legendside <- match.arg(legendside)

  dat <- enkey(dat, key, ...)

  if (with_density && !is.null(facet_aes)) {
    warning("Marginal densities not supported with facets. Facets disabled")
    facet_aes <- NULL
  }

  if (length(axes) == 3L && !is.null(facet_aes)) {
    stop("Can not facet a 3d plot")
  }

  if (is.null(hover)) {
    hover <- c(color_aes, shape_aes, size_aes)
  }

  xx <- with_aesthetics(dat, color_aes = color_aes, color_map = color_map,
                        shape_aes = shape_aes, shape_map = shape_map,
                        size_aes = size_aes, size_map = size_map,
                        hover = hover)

  has_legend <- !is.null(color_aes) ||
    !is.null(shape_aes) ||
    !is.null(size_aes)

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
                      has_legend = has_legend, plot_type = "scatter",
                      axes = axes, xf = xf, yf = yf, zf = zf,
                      with_density = with_density,
                      marker_size = marker_size, .color = .color,
                      .colors = .colors, .shape = .shape,
                      .shapes = .shapes, ...,
                      width = width, height = height, flat = flat,
                      showlegend = showlegend, legendside = legendside,
                      xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
                      key = key, event_source = event_source)
  out <- list(plot = plot, input_data = dat, params = list())

  if (webgl) {
    out$plot <- toWebGL(out$plot)
  }
  class(out) <- c("FacileScatterViz", "FacileViz")
  out
}

#' The lowest-level scatterplot function that generates a plotly plot object.
#' The caller will decorate the result with all the appropriate class info
#' and such
#'
#' @noRd
#' @importFrom plotly
#'   add_markers
#'   colorbar
#'   config
#'   layout
#'   plot_ly
#'   subplot
.fscatterplot <- function(xx, axes, xf, yf, zf, with_density,
                          facet_aes, facet_nrows, marker_size,
                          .color, .colors,
                          .shape, .shapes, ...,
                          legendside = NULL,
                          height = NULL, width = NULL, flat = FALSE,
                          xlabel, ylabel, zlabel, key, event_source,
                          legendgroup = NULL, showlegend = TRUE, title = NULL) {
  xaxis <- list(title = if (!is.null(xlabel)) xlabel[1L] else axes[1L])
  yaxis <- list(title = if (!is.null(ylabel)) ylabel[1L] else axes[2L])
  if (!is.null(key)) key <- paste0("~", key)

  nofacet <- missing(facet_aes) || length(axes) == 3L

  lgroup <- local({
    if (nofacet) {
      out <- NULL
    } else {
      lcols <- c(".color_aes.variable", ".shape_aes.variable")
      out <- lcols[sapply(lcols, function(v) !is.null(xx[[v]]))]
      if (length(out) == 0) {
        out <- NULL
      } else if (length(out) == 1L) {
        out <- paste("~", out)
      } else {
        out <- sprintf("~ paste(%s)", paste(out, collapse = ","))
      }
    }
    out
  })

  amap <- aes_map(xx)
  lname <-
  if (length(axes) == 2L) {
    p <- plot_ly(xx, x = formula(xf), y = formula(yf),
                 height = height, width = width,
                 source = event_source, key = formula(key),
                 legendgroup = if (is.null(lgroup)) NULL else formula(lgroup),
                 showlegend = showlegend)
    if (is.function(.colors)) {
      # continous color (using colorscale). The way we do this now sucks when
      # shooting data to plotly: I can't pass plotly a custom colorRamp-like
      # function, so just using its default (viridis)
      p <- add_markers(p, type = "scatter",
                       color = .color,
                       symbol = .shape, symbols = .shapes,
                       marker = list(size = marker_size),
                       text = ~.hover)
      if (showlegend) {
        # for colorbars over a zscore, you can do
        # colorbar(p, limits = c(-1, 1))
        color_colname <- attr(amap$color, "columns")[["label"]]
        p <- colorbar(p, title = color_colname)
      }
    } else {
      # we've got our own mapping: discrete colors seems to work fine with
      # plotly the way we currently do it.
      p <- add_markers(p, type = "scatter",
                       color = .color,
                       colors = .colors,
                       symbol = .shape, symbols = .shapes,
                       marker = list(size = marker_size),
                       text = ~.hover)
    }
    p <- config(p, displaylogo = FALSE)
    p <- layout(p, xaxis = xaxis, yaxis = yaxis, dragmode = "select")
  } else {
    if (flat) {
      plots <- list()
      axf <- list(xf, yf, zf)
      lbls <- list(xlabel, ylabel, zlabel)
      idxs <- list(c(1,2), c(1,3), c(2,3))
      plots <- lapply(idxs, function(pair) {
        xi <- pair[[1]]
        yi <- pair[[2]]
        pp <- plot_ly(xx, x = formula(axf[[xi]]), y = formula(axf[[yi]]),
                      source = event_source, key = formula(key),
                      # showlegend = FALSE,
                      height = height, width = width,
                      legendgroup = lgroup, showlegend = showlegend)
        pp <- add_markers(pp, type = "scatter",
                          color = .color, colors = .colors,
                          symbol = .shape, symbols = .shapes,
                          marker = list(size = marker_size),
                          text = ~.hover)
        pp <- layout(pp, xaxis = lbls[[xi]], yaxis = lbls[[yi]],
                     dragmode = "select")
        pp
      })
      p <- subplot(plots, nrows = 1,
                   shareX = FALSE, shareY = FALSE, titleX = TRUE,
                   titleY = TRUE, which_layout = "merge")
      p <- unify_legend(p)
    } else {
      mtype <- "scatter3d"
      # camera settings from:
      # https://plot.ly/python/3d-camera-controls/
      # https://plot.ly/~klara.roehrl/225/camera-controls-eye-x01-y25-z01/#plot
      zaxis <- list(title = if (!is.null(zlabel)) zlabel[1L] else axes[3L])
      camera <- list(
        center = list(x = 0, y = 0, z = 0),
        up = list(x = 0, y = 0, z = 1),
        eye = list(x = 0.2, y = 2, z = 0.1))
      scene <- list(xaxis = xaxis, yaxis = zaxis, zaxis = yaxis,
                    camera = camera)
      p <- plot_ly(xx, x = formula(xf), y = formula(zf), z = formula(yf),
                   source = event_source, key = formula(key),
                   height = height, width = width, showlegend = showlegend)
      p <- add_markers(p, type = "scatter3d",
                       color = .color, colors = .colors,
                       symbol = .shape, symbols = .shapes,
                       marker = list(size = marker_size),
                       text = ~.hover)
      p <- config(p, displaylogo = FALSE)
      p <- layout(p, scene = scene, dragmode = "select")
    }
  }

  p <- layout(p, dragmode = "select")
  if (nofacet) {
    if (isTRUE(legendside == "bottom")) {
      p <- layout(p, legend = list(orientation = "h", y = -0.3))
    } else if (isTRUE(legendside == "none")) {
      p <- layout(p, showlegend = FALSE)
    }
  }

  if (test_string(title)) {
    p <- layout(p, title = title)
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
                                 showlegend = TRUE,
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
               hover = hover, ...,
               showlegend = showlegend,
               marker_size = marker_size, sizes = sizes,
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
