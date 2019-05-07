#' Drives faceting-like behavior for (internal) plotly-generating functions.
#'
#' Other FacileViz functions call this function to drive faceting behavior.
#' This function is largely meant to be internal and is purposefully not
#' exported yet.
#'
#' @importFrom plotly add_annotations subplot
#'
#' @param facet_aes The name of a column in `dat` to use for faceting. If this
#'   is a length-2 vector, then we try to do a facet_grid (undone.)
#' @inheritParams plotly::subplot
maybe_facet <- function(plotfn, plotdat, facet_aes, nrows = NULL,
                        has_legend = FALSE, ...,
                        # height and width of a plot w/o facets, for squarish
                        # plots, we usually want the width a littler larger than
                        # the height.
                        height = 600,
                        width = height + (height / 6),
                        facet_height = 300,
                        facet_width =  370,
                        # subplot parameters
                        widths = NULL, heights = NULL, margin = 0.02,
                        shareX = TRUE, shareY = TRUE, titleX = shareX,
                        titleY = shareY, which_layout = "merge") {
  if (!is.null(facet_aes)) {
    assert_character(facet_aes, min.len = 1L, max.len = 2L)
    if (length(facet_aes) == 2L) {
      stop("facet_grid-like functionality not yet implemented")
    }
    assert_subset(facet_aes, names(plotdat))

    sdat <- split(plotdat, plotdat[[facet_aes]])
    if (is.null(nrows)) {
      nrows <- ceiling(length(sdat) / 3)
    }
    # This is how the number of columns is calculated within the subplot
    # function (plotly v4.8.0)
    ncols <- ceiling(length(sdat) / nrows)

    # In plotly 4.9.0, although the height and width are set in the indivdual
    # plot_ly calls, we need to pass in the total height and width of the
    # faceted plot, not the dimmensions of the individual plots.
    width <- (ncols * facet_width) + (ncols * 10) # last term for margin
    height <- (nrows * facet_height) + (ncols * 10)

    facets <- sapply(names(sdat), function(name) {
      fdat <- sdat[[name]]
      i <- match(name, names(sdat))
      showlegend <- has_legend && name == names(sdat)[1L]
      # message(name, ": ", showlegend)
      out <- plotfn(fdat, facet_aes = NULL, ..., width = width,
                    height = height, showlegend = showlegend)
      # https://github.com/plotly/plotly.js/blob/master/src/components/annotations/attributes.js
      add_annotations(out, text = sprintf("<b>%s</b>", name),
                      x = 0, xref = "paper", xanchor = "left",
                      y = 1, yref = "paper", yanchor = "top", yshift = 15,
                      showarrow = FALSE)
    }, simplify = FALSE)
    p <- subplot(facets, nrows = nrows, widths = widths,
                 heights = heights, margin = margin, shareX = shareX,
                 shareY = shareY, titleX = titleX, titleY = titleY,
                 which_layout = which_layout)
    p <- unify_legend(p)
  } else {
    if (has_legend) width <- width + 100
    p <- plotfn(plotdat, ..., width = width, height = height)
  }

  p
}

#' Hacks into plotly internals merge legends across subplots.
#'
#' This is important when the subplot that has `showlegend = TRUE` doesn't have
#' all the data points to cover the entirety of the data/legend key.
#'
#' This is SUUUUUUUUPER brittle. Also note that your `plot_ly` calls need to
#' use `legendgroup` correctly.
#'
#' @export
#' @param x plotly object
#' @return A mangled `x`
#' @examples
#' dat <- tibble(
#'   x = 1:6,
#'   y = 1:6,
#'   a = c("a", "a", "b", "c", "b", "d"),
#'   b = c("x", "y", "y", "y", "z", "z"))
#' colors <- c(a = "blue", b = "red", c = "green", d = "orange")
#' plots <- sapply(unique(dat$b), function(val) {
#'   xdat <- filter(dat, b == val)
#'   # You need go guess the appropriate subplot to set `showlegend = TRUE`,
#'   # but there are none that have all values of `a`.
#'   plot_ly(xdat, x = ~x, y = ~y, legendgroup = ~a, showlegend = val == "y") %>%
#'     add_markers(type = "scatter", color = ~a, colors = colors)
#' }, simplify = FALSE)
#'
#' # compare
#'
#' (sb <- subplot(plots))
#' unify_legend(sb)
unify_legend <- function(x, ...) {
  assert_class(x, "plotly")
  x <- plotly::plotly_build(x)
  grps <- lapply(x$x$data, '[[', "legendgroup")
  showit <- !duplicated(unlist(grps))
  for (i in seq(x$x$data)) {
    x$x$data[[i]]$showlegend <- showit[i]
  }
  x
}
