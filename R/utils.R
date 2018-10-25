# Random utility functions

#' @noRd
#' @export
is.categorical <- function(x, ...) {
  is(x, "character") || is(x, "factor")
}
