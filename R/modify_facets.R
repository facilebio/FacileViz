#' Update the styling of the individual facets of a ggplot.
#'
#' Change label and background/fill colors of the ribbons.
#'
#' The nitty-gritty of this function (it's rough) was inspired from many a
#' google search, with particularly helpful hints from CharlotteWoolley here:
#' https://github.com/tidyverse/ggplot2/issues/2096#issuecomment-388545380
#'
#' @export
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @param x The ggplot object. We assume this has already been faceted.
#' @param specs A list-of-list object that defines the parameters to update.
#'   `names(specs)` are the labels of the facets to update, the values are a
#'   list of things:
#'     * `$fill`: background color
#'     * `$border`: stroke around the ribbon
#'     * `$color`: the color of the text.
#'     * `$label`: the text of the label in the facet ribbon
#' @return a [gtable::gtable()] object, build by [ggplot2::ggplot_gtable()].
#'   You can plot this using via [grid::grid.draw()]
#' @examples
#' library(ggplot2)
#' x <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~ cyl)
#' specs <- list(
#'   "4" = list(
#'     label = "four",
#'     fill = "red",
#'     color = "white"),
#'   "5" = list(
#'     fill = "purple",
#'     color = "yellow"))
#' updated <- modify_facets(x, specs)
#' if (interactive()) {
#'   grid::grid.draw(updated)
#' }
modify_facets <- function(x, specs, ...) {
  gt <- ggplot_gtable(ggplot_build(x))

  # Find the index of the grobs that define the facet titles/ribbons on a
  # facet_wrap
  tops <- grep('strip-t', gt$layout$name)
  sides <- grep('strip-l', gt$layout$name)
  # TODO: we aren't disambiguating between row and colum labels
  any.facet <- c(tops, sides)

  # TODO: use grid::grid.edit and grid::gPath functions to make this simpler?
  # https://stackoverflow.com/a/34758629/83761
  for (i in any.facet) {
    igrob <- gt$grobs[[i]]$grobs[[1]]
    ichildren <- igrob$children
    idx.bg <- grep('rect', igrob$childrenOrder)
    idx.title <- grep('text.*title', igrob$childrenOrder)
    ilabel <- ichildren[[idx.title]]$children[[1]]$label

    info <- specs[[ilabel]]
    if (is.null(info)) {
      # No modification information for this facet
    } else {
      if (!is.null(info$fill)) {
        ichildren[[idx.bg]]$gp$fill <- info$fill
      }
      if (!is.null(info$border)) {
        ichildren[[idx.bg]]$gp$col <- info$border
      }
      if (!is.null(info$label)) {
        ichildren[[idx.title]]$children[[1]]$label <- info$label
      }
      if (!is.null(info$color)) {
        ichildren[[idx.title]]$children[[1]]$gp$col <- info$color
      }
      gt$grobs[[i]]$grobs[[1]]$children <- ichildren
    }
  }

  gt
}
