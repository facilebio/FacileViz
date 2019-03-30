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
maybe_facet <- function(plotfn, plotdat, facet_aes, nrows = NULL, ...,
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
    arr.inds <- arrayInd(seq(sdat), c(nrows, ncols))

    facets <- sapply(names(sdat), function(name) {
      fdat <- sdat[[name]]
      i <- match(name, names(sdat))
      # We only need to bump the titleX position if only have one row?
      title.offset <- (arr.inds[i, 2] - 1) * (margin * 2.5)
      title.offset <- title.offset * (nrows == 1)
      out <- plotfn(fdat, facet_aes = NULL, ...)
      add_annotations(out, text = sprintf("<b>%s</b>", name),
                      x = title.offset, y = 1,
                      xref = "paper", yref = "paper", xanchor = "middle",
                      yanchor = "middle", showarrow = FALSE)
    }, simplify = FALSE)
    p <- subplot(facets, nrows = nrows, widths = widths,
                 heights = heights, margin = margin, shareX = shareX,
                 shareY = shareY, titleX = titleX, titleY = titleY,
                 which_layout = which_layout)
  } else {
    p <- plotfn(plotdat, ...)
  }

  p
}
