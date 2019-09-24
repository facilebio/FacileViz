#' Sets the "key" column for a data.frame
#'
#' This column is used to match against when users brush elements on a plot.
#' If multiple column names are given, then a new `key_column` is appended
#' to `x` which is constructed by the combination of the columns specified
#' in `key`. The name of the column that was finally used as a key is stored
#' `attr(result, "key_column")`
#'
#' @export
#' @param x the data.frame
#' @param key The name(s) of the columns to use as a unique key into the
#'   dataset. If key is `NULL` (default), then x[[key_column]] is added and set
#'   to `1:nrow(x)`.
#' @return `x` with the appropriate key set.
enkey <- function(x, key = NULL, key_column = ".data_key.", ...) {
  assert_multi_class(x, c("data.frame", "tbl"))
  assert_subset(key, colnames(x))
  assert_string(key_column)

  if (is.null(key) || length(key) > 1L) {
    # We need to add a new key column to the `x`, so let's make sure we aren't
    # blowing out an already-existing column
    if (test_choice(key_column, colnames(x))) {
      use.me <- paste0(key_column, ncol(x) + 1L, ".key", collapse = "")
      use.me <- setdiff(use.me, colnames(x))
      warning("New `key_column` already exists in `x`, using: ", use.me)
      key_column <- use.me
    }
  }

  if (is.null(key)) {
    x[[key_column]] <- seq(nrow(x))
  } else {
    is.cat <- sapply(x[, key, drop = FALSE], function(vals) {
      is.character(vals) || is.factor(vals) || is.integer(vals)
    })
    if (!all(is.cat)) {
      stop("Only discrete column types are valid keys, these aren't: ",
           paste(names(is.cat)[!is.cat], collapse = ","))
    }
    if (length(key) > 1) {
      x <- tidyr::unite(x, {{key_column}}, {{key}}, sep = "_", remove = FALSE)
    } else {
      key_column <- key
    }
  }

  if (any(duplicated(x[[key_column]]))) {
    stop("There are duplicated entries in the key_column")
  }

  attr(x, "key_column") <- key_column
  x
}
