#' Draws interactive 2 or 3d scatterplots over data
#'
#' Use the `event_source` parameter to link selection/brushing of the plot
#' to a listener.
#'
#' @rdname fscatterplot
#' @export
#' @param x a data object
#' @param axes the definition of the x,y,z axes
#' @return a plotly object
#' @examples
#' dat <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100),
#'                   class = sample(c("g1", "g2", "g3"), 100, replace = TRUE),
#'                   grp = sample(c("n", "o", "p"), 100, replace = TRUE))
#' fscatterplot(dat, c("a", "b"), color_aes = "class", hover = "class")
#' fscatterplot(dat, c("a", "b"), color_aes = "class", hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b", "c"), color_aes = "class",
#'              hover = c("class", "c"))
#' fscatterplot(dat, c("a", "b"), color_aes = "class", shape_aes = "grp",
#'              hover = c("class", "c"),
#'              facet_aes = "class")
fscatterplot <- function(dat, axes,
                         color_aes = NULL, color_map = NULL,
                         shape_aes = NULL, shape_map = NULL,
                         size_aes = NULL, size_map = NULL,
                         facet_aes = NULL, facet_nrow = NULL,
                         hover = NULL,
                         ...,
                         xlabel = NULL, ylabel = NULL, zlabel = NULL,
                         # direct plot_ly params:
                         marker_size = 8,
                         sizes = c(10, 100),
                         event_source = "A") {
  UseMethod("fscatterplot", dat)
}

fscatterplot.default <- function(dat, axes,
                                 color_aes = NULL, color_map = NULL,
                                 shape_aes = NULL, shape_map = NULL,
                                 size_aes = NULL, size_map = NULL,
                                 facet_aes = NULL, facet_nrow = NULL,
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
               facet_aes = facet_aes, facet_nrow = facet_nrow,
               hover = hover, ..., marker_size = marker_size, sizes = sizes,
               event_source = event_source)
}

fscatterplot.FaclieDataStore <- function(dat, axes,
                                         color_aes = NULL, color_map = NULL,
                                         shape_aes = NULL, shape_map = NULL,
                                         size_aes = NULL, size_map = NULL,
                                         facet_aes = NULL, facet_nrow = NULL,
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
                                   facet_aes = NULL, facet_nrow = NULL,
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
               facet_aes = facet_aes, facet_nrow = facet_nrow,
               hover = hover, ...,
               xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
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
                             facet_aes = NULL, facet_nrow = NULL,
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
                          facet_nrow = facet_nrow, ...,
                          xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
                          marker_size = marker_size,
                          sizes = sizes, event_source = event_source)
}

#' @rdname fscatterplot
#' @method fscatterplot data.frame
#' @export
fscatterplot.data.frame <- function(dat, axes,
                                    color_aes = NULL, color_map = NULL,
                                    shape_aes = NULL, shape_map = NULL,
                                    size_aes = NULL, size_map = NULL,
                                    facet_aes = NULL, facet_nrow = NULL,
                                    hover = NULL, ...,
                                    xlabel = NULL, ylabel = NULL, zlabel = NULL,
                                    marker_size = 8,
                                    sizes = c(10, 100),
                                    event_source = "A") {
  assert_character(axes, min.len = 2L, max.len = 3L)
  assert_subset(axes, names(dat))
  assert_subset(c(color_aes, shape_aes, size_aes, facet_aes, hover), names(dat))
  # if (!is.null(facet_aes)) assert_int(facet_nrow)

  xx <- dat
  xx <- with_color(xx, color_aes, aes_map = color_map, ...)
  xx <- with_shape(xx, shape_aes, aes_map = shape_map, ...)
  # xx <- with_size(xx, size_aes, ...)
  # xx <- with_hover(xx, hover_aes, ...)

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

  .fscatterplot(dat, xx, axes, xf, yf, zf, facet_aes, facet_nrow,
                marker_size, .color, .colors,
                .shape, .shapes, ...,
                xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
                event_source = event_source)
}

.fscatterplot <- function(dat, xx, axes, xf, yf, zf, facet_aes, facet_nrow,
                          marker_size, .color, .colors,
                          .shape, .shapes, ...,
                          xlabel, ylabel, zlabel, event_source) {
  if (!is.null(facet_aes)) {
    xxx <- split(xx, xx[[facet_aes]])
    if (is.null(facet_nrow)) {
      # Let's assume we want a maximum of three plots wide.
      facet_nrow <- ceiling(length(xxx) / 3)
    }
    facets <- sapply(names(xxx), function(name) {
      xdat <- xxx[[name]]
      out <- .fscatterplot(xdat, xx = xx, axes = axes, xf = xf, yf = yf,
                           zf = zf, facet_aes = NULL,
                           facet_nrow = facet_nrow,
                           marker_size = marker_size,
                           .color = .color, .colors = .colors,
                           .shape = .shape, .shapes = .shapes, ...,
                           xlabel = xlabel, ylabel = ylabel, zlabel = zlabel,
                           event_source = event_source,
                           legendgroup = .color,
                           showlegend = name == names(xxx)[1L])
      add_annotations(out[["plot"]], text = name, x = 0.5, y = 1,
                      xref = "paper", yref = "paper", xanchor = "middle",
                      yanchor = "middle", showarrow = FALSE)
    }, simplify = FALSE)
    # annos <- sapply(names(facets), function(name) list(text = name))
    p <- subplot(facets, nrows = facet_nrow, ...)
  } else {
    xaxis <- list(title = if (!is.null(xlabel)) xlabel[1L] else axes[1L])
    yaxis <- list(title = if (!is.null(ylabel)) ylabel[1L] else axes[2L])
    if (length(axes) == 2L) {
      p <- plot_ly(xx, x = formula(xf), y = formula(yf),
                   type = "scatter",  mode = "markers",
                   marker = list(size = marker_size),
                   color = .color, colors = .colors,
                   symbol = .shape, symbols = .shapes,
                   text = ~.hover, source = event_source,
                   ...)
      p <- layout(p, xaxis = xaxis, yaxis = yaxis)
    } else {
      zaxis <- list(title = if (!is.null(zlabel)) zlabel[1L] else axes[3L])
      scene <- list(xaxis = xaxis, yaxis = yaxis, zaxis = zaxis)

      p <- plot_ly(xx, x = formula(xf), z = formula(yf), y = formula(zf),
                   type = "scatter3d", mode = "markers",
                   color = .color, colors = .colors,
                   symbol = .shape, symbols = .shapes,
                   marker = list(size = marker_size),
                   text = ~.hover, source = event_source)
      p <- layout(p, scene = scene)
      p <- layout(p, dragmode = "select")
    }
  }

  out <- list(plot = p, input_data = dat, params = list())
  class(out) <- c("FacileScatterViz", "FacileViz")
  out
}
