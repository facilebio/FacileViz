#' Checks if the return value from a selectInput looks like it's unselected
#'
#' For some reason, sometimes a selectInput returns `NULL` and other times
#' it returns `""`, so I'm just making this utility function to deal with that
#'
#' NOTE: This is really a function that is used by shiny modules, but instead
#' of having every shiny function check if something is "unselected" and setting
#' it to NULL, we move that functionality in here so that internal vizualization
#' functions can do that just once
#'
#' @export
#' @param value The (character) object returned from a `selectInput`
#' @return a logical (TRUE/FALSE) indicating if something is "selected"
unselected <- function(value, ignore = c("---", "__initializing__", "")) {
  if (is.null(value)) return(TRUE)
  # Otherwise this is a character
  if (is(value, "data.frame") || is(value, "tbl")) {
    val <- nrow(value) == 0
  } else {
    val <- length(value) == 0L || 
      all(sapply(value, function(v) nchar(v) == 0L)) || 
      all(value %in% ignore)
  }
  val
}
