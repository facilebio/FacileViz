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
      out <- plotfn(fdat, facet_aes = NULL, ..., width = width,
                    height = height)
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
  } else {
    if (has_legend) width <- width + 100
    p <- plotfn(plotdat, ..., width = width, height = height)
  }

  p
}
