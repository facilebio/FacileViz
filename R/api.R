# A FacileViz object (fP) includes the following elements:
#
# * `fp$facile_analysis`: The `FacileAnalysisResult` object that the plot was
#   generated from. This is made accessible by alling `fanalysis_result(fp)`.
# * `fp$input_data`: A slimmed down version of the data used for the final plot.
#   A bigger version of this data should be available via:
#   `fp %>% fanalysis_result() %>% tidy()`
# * `fp$plot`: The plotly (for now) object that is the actual plot itself. This
#   is made accessible via `plot(fp)` method
# * `fp$params`: The parameters used to configure the plot, made accessible via
#   `fparams(fp)` (TODO: make fparams S3)

# Getters and Setters ----------------------------------------------------------
#' Retrieve the input object from a FacileViz
#' @export
input_data <- function(x, ...) UseMethod("input_data", x)

#' Extracts the data used for plotting
#' @export
plot_data <- function(x, ...) UseMethod("plot_data", x)

#' Extract a FacileAnalysisResult from something
#' @export
analysis_result <- function(x, ...) UseMethod("analysis_result", x)

#' Extract (plotly)-plot object from FacilPlots objects
#'
#' This creates an
#'
#' @noRd
#'
#' @export
#' @method plot FacileViz
plot.FacileViz <- function(x, y, ...) {
  assert_class(x, "FacileViz")
  x$plot
}

#' @export
#' @method analysis_result FacileViz
analysis_result.FacileViz <- function(x, ...) {
  assert_class(x, "FacileViz")
  x$facile_analysis
}

#' Extract a tidy data.frame from an `iresult`
#'
#' @noRd
#' @export
#' @method tidy FacileViz
#'
#' @param x a `FacileViz` object
#' @return a tidy data.frame of the results from an immersive analysis
tidy.FacileViz <- function(x, ...) {
  expect_class(x, "FacileViz")
  fr <- analysis_result(x)
  tidy(fr)
}

#' Default print method for a FacileViz object for printing to the R console
#'
#' @noRd
#' @export
#' @method print FacileViz
print.FacileViz <- function(x, ..., view = interactive()) {
  # since plot(x) returns a plotly object, the print method delegates to
  # `htmlwidgets::print.htmlwdiget` like any ordinary plotly object
  invisible(print(plot(x), ..., view = view))
}

# Letting the knit_print method pass through from
# plotly -> htmlwidgets::knit_print seems to be doing the right thing
# for now
# knit_print.Viz <- function(x, ..., options = NULL) {
#   # delegates to htmlwidgets::knit_print.htmlwdiget
#   knitr::knit_print(plot(x))
# }
#

