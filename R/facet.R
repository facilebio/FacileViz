maybe_facet_wrap <- function(plotfn, plotdat, facet_aes, facet_nrows = NULL, ...,
                             widths = NULL, heights = NULL, facet_margin = 0.02,
                             shareX = TRUE, shareY = TRUE, titleX = shareX,
                             titleY = shareY, which_layout = "merge") {
  if (!is.null(facet_aes)) {
    assert_string(facet_aes)
    assert_subset(facet_aes, names(plotdat))

    sdat <- split(plotdat, plotdat[[facet_aes]])
    if (is.null(facet_nrows)) {
      facet_nrows <- ceiling(length(sdat) / 3)
    }
    # This is how the number of columns is calculated within the subplot
    # function (plotly v4.8.0)
    ncols <- ceiling(length(sdat) / facet_nrows)
    arr.inds <- arrayInd(seq(sdat), c(facet_nrows, ncols))

    facets <- sapply(names(sdat), function(name) {
      fdat <- sdat[[name]]
      i <- match(name, names(sdat))
      # We only need to bump the titleX position if only have one row?
      title.offset <- (arr.inds[i, 2] - 1) * (facet_margin * 2.5)
      title.offset <- title.offset * (facet_nrows == 1)
      out <- plotfn(fdat, facet_aes = NULL, ...)
      add_annotations(out, text = sprintf("<b>%s</b>", name),
                      x = title.offset, y = 1,
                      xref = "paper", yref = "paper", xanchor = "middle",
                      yanchor = "middle", showarrow = FALSE)
    }, simplify = FALSE)
    p <- subplot(facets, nrows = facet_nrows, widths = widths,
                 heights = heights, margin = facet_margin, shareX = shareX,
                 shareY = shareY, titleX = titleX, titleY = titleY,
                 which_layout = which_layout)
  } else {
    p <- plotfn(plotdat, ...)
  }

  p
}
