#' Create an interactive boxplot
#'
#' Look at boxplot section of the new book for guidance.
#' https://plotly-r.com/boxplots.html
#'
#' For now, instead of enabling a "grouping" variable, `fboxplot` we just enable
#' what is possible via faceting. See the examples section for inspiration on
#' how we can use a facet to approximate a "grouped" boxplot.
#'
#' @section Mapping aesthetics to geometries:
#'
#' When `with_points = FALSE`, the `color_aes` can only be set to the value of
#' the `x` categorical axis, if anything at all.
#'
#' When `with_points = TRUE`, the boxes will be colored white, and the points
#' will be colored by the aesthetic.
#'
#' I can't figure out how to get the shape stuff working in conjunction with
#' the `unify_legend` logic required to make faceted legends work in the more
#' general case. This is a WIP.
#'
#' @export
#'
#' @param dat the data source
#' @param x the (categorical) covariate for the x axis
#' @param y the (real valued) values for the y
#' @param group_aes column in `dat` to group barplots by (not yet implemented)
#'
#' @examples
#' data("diamonds", package = "ggplot2")
#' dat <- dplyr::sample_n(diamonds, 2000)
#'
#' # Grouped boxplots
#' plot_ly(dat, x = ~cut, y = ~price) %>%
#'   add_boxplot(color = ~clarity) %>%
#'   layout(boxmode = "group")
#'
#' # Grouped boxlpot by facet
#' plots <- dat %>%
#'   group_by(cut) %>%
#'   do(plot = {
#'     name <- as.character(.$cut[1L])
#'     plot_ly(., x = ~as.numeric(clarity), y = ~price, legendgroup = ~clarity,
#'             showlegend = .$cut[1] == diamonds$cut[1]) %>%
#'       add_boxplot(pointpos = 0,
#'                   boxpoints = FALSE,
#'                   color = ~ clarity,
#'                   # line = list(color = "black"),
#'                   showlegend = FALSE) %>%
#'       add_markers(x = ~jitter(as.numeric(clarity)), y = ~price,
#'                   color = ~ clarity) %>%
#'       add_annotations(text = sprintf("<b>%s</b>", name),
#'                       x = 0, xref = "paper", xanchor = "left",
#'                       y = 1, yref = "paper", yanchor = "top", yshift = 15,
#'                       showarrow = FALSE) %>%
#'       layout(xaxis = list(tickvals = 1:8, ticktext = levels(diamonds$clarity)))
#'   })
#' subplot(plots, nrows = 1, shareY = TRUE)
#'
#' # Grouped boxplot with fboxplot
#' fboxplot(dat, "clarity", "price", color_aes = "cut", with_points = TRUE)
#' fboxplot(dat, "clarity", "price", facet_aes = "cut")
#' fboxplot(dat, "clarity", "price", facet_aes = "cut", with_points = TRUE)
#' fboxplot(dat, "clarity", "price", facet_aes = "cut",
#'          color_aes = "clarity", with_points = TRUE)
fboxplot <- function(dat, x, y, with_points = FALSE, group_aes = NULL,
                     color_aes = NULL, color_map = NULL,
                     shape_aes = NULL, shape_map = NULL,
                     size_aes = NULL, size_map = NULL,
                     facet_aes = NULL, facet_nrows = NULL,
                     hover = NULL,
                     na_x = c("remove", "keep"),
                     na_y = c("remove", "keep"),
                     ...,
                     xlabel = NULL, ylabel = NULL,
                     # direct plot_ly params:
                     marker_size = 8,
                     height = 600,
                     width = height + (height / 6),
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
                                na_x = c("remove", "keep"),
                                na_y = c("remove", "keep"),
                                ...,
                                legendside = NULL,
                                xlabel = NULL, ylabel = NULL,
                                # direct plot_ly params:
                                marker_size = 8,
                                height = 600,
                                width = height + (height / 6),
                                sizes = c(10, 100),
                                pointpos = -1.8,
                                key = NULL,
                                event_source = "A") {
  assert_string(x)
  assert_string(y)
  assert_subset(c(x, y), names(dat))
  assert_categorical(dat[[x]])
  na_x <- match.arg(na_x)
  na_y <- match.arg(na_y)
  if (!is.null(key)) {
    assert_choice(key, colnames(dat))
  }

  if (na_x == "keep") {
    vals <- dat[[x]]
    isna <- is.na(vals)
    if (any(isna)) {
      if (is.factor(vals)) {
        lvls <- c(levels(vals), "NA.")
        vals <- as.character(vals)
      } else {
        lvls <- NULL
      }
      vals[isna] <- "NA."
      if (!is.null(lvls)) {
        vals <- factor(vals, lvls)
      }
      dat[[x]] <- vals
    }
  }

  if (!is.factor(dat[[x]])) {
    dat[[x]] <- factor(dat[[x]])
  }
  dat[[x]] <- droplevels(dat[[x]])

  assert_numeric(dat[[y]])

  xtickvals <- seq(nlevels(dat[[x]]))
  xticktext <- levels(dat[[x]])

  if (!is.null(color_aes) && !with_points && color_aes != x) {
    warning("You can't color a boxplot without points by anything other than ",
            "the category on the x-axis")
    color_aes <- x
  }

  if (!with_points && !is.null(shape_aes)) {
    warning("You can't specify a shape aesthetic when with_points is FALSE")
    shape_aes <- NULL
  }

  has_legend <- !is.null(color_aes) ||
    !is.null(shape_aes) ||
    !is.null(size_aes)

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

  has_legend <- !is.null(color_aes) ||
    !is.null(shape_aes) ||
    !is.null(size_aes)

  plot_type <- if (with_points) "box+points" else "box"

  plot <- maybe_facet(.fboxplot, xx, facet_aes, facet_nrows,
                      has_legend = has_legend, plot_type = plot_type,
                      x = x, y = y, with_points = with_points,
                      group_aes = group_aes, marker_size = marker_size,
                      .color = .color, .colors = .colors,
                      .shape = .shape, .shapes = .shapes,
                      ...,
                      width = width, height = height,
                      xlabel = xlabel, ylabel = ylabel,
                      legendside = legendside,
                      pointpos = pointpos,
                      xtickvals = xtickvals, xticktext = xticktext,
                      key = key, event_source = event_source)

  out <- list(plot = plot, input_data = dat, params = list())
  class(out) <- c("FacileBoxPlotViz", "FacileViz")
  out

  out
}

#' The lowest-level scatterplot funciton that generates a plotly plot object.
#' The caller will decorate the result with all the appropriate class info
#' and such
#'
#' @noRd
#' @importFrom plotly add_boxplot config layout plot_ly
.fboxplot <- function(xx, x, y, with_points, facet_aes, group_aes, facet_nrows,
                      marker_size, .color, .colors, .shape, .shapes, ...,
                      height = NULL, width = NULL,
                      xlabel, ylabel, pointpos,
                      xtickvals, xticktext,
                      legendgroup = NULL, showlegend = TRUE,
                      legendside = NULL,
                      key, event_source) {
  xaxis <- list(title = xlabel)
  yaxis <- list(title = ylabel)

  if (!is.null(key)) key <- paste0("~", key)
  nofacet <- missing(facet_aes)

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
  })

  yf <- paste0("~", y)
  if (with_points) {
    xf <- sprintf("~as.numeric(%s)", x)
    pf <- sprintf("~jitter(as.numeric(%s))", x)
    if (is.null(xlabel)) xlabel <- x # otherwise it's as.numeric()
    xaxis <- list(tickvals = xtickvals, ticktext = xticktext, title = xlabel)

    plt <- plot_ly(xx, x = formula(xf), y = formula(yf), text = ~.hover,
                   showlegend = showlegend, height = height, width = width,
                   source = event_source, key = formula(key)) %>%
      add_boxplot(boxpoints = FALSE,
                  line = list(color = "black"),
                  fillcolor = "white",
                  showlegend = FALSE) %>%
                  # legendgroup = if (is.null(lgroup)) NULL else formula(lgroup)) %>%
      add_markers(x = formula(pf), y = formula(yf),
                  color = .color, colors = .colors,
                  legendgroup = if (is.null(lgroup)) NULL else formula(lgroup)) %>%
      layout(xaxis = xaxis, yaxis = yaxis)

  } else {
    xf <- paste0("~", x)
    plt <- plot_ly(xx, x = formula(xf), y = formula(yf), text = ~.hover,
                   showlegend = showlegend, height = height, width = width,
                   source = event_source, key = formula(key)) %>%
      add_boxplot(boxpoints = "outliers", pointpos = 0,
                  color = .color, colors = .colors,
                  legendgroup = if (is.null(lgroup)) NULL else formula(lgroup)) %>%
      layout(xaxis = xaxis, yaxis = yaxis)

  }
  plt <- layout(plt, dragmode = "select")
  if (nofacet && isTRUE(legendside == "bottom")) {
    plt <- layout(plt, legend = list(orientation = "h", y = -0.3))
  }
  plt <- config(plt, displaylogo = FALSE)
  plt
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
