#' the valid aesthetic names that can be set
#' @noRd
aes_names <- function() {
  c("color", "size", "shape")
}

#' Set and retrieve mappings for facile objects.
#'
#' @description
#' * `aes_map` retrieves all aesthetic maps, or the map for an individual
#'   aesthetic when `aes_name` is specified.
#' * `aes_map()<-` resets the map, wholesale.
#'
#' @export
#' @rdname aes_map
#' @seealso [create_color_map()]
#'
#' @param x an orbitrary object
#' @param aes_name optional aes_name you want to individually set or retrieve,
#'   otherweise entire aesthetic map list is set/returned
#' @return the getter returns the whole aesthetic map when `is.null(aes_name)`,
#'   or the specific mapping for the named aesthetic. The setter sets the
#'   specific `aes_name` when specified, otherwise sets the aesthetics in
#'   the named `value` list
#'
#' @examples
#' x <- data.frame(a = 1:5, b = letters[1:5])
#' aes_map(x, "color") <- create_color_map(x$b)
#' aes_map(x, "size") <- "all sizes"
#' aes_map(x, "color")
#'
#' # simultaneously updates color map and adds shape map
#' aes_map(x) <- list(color = create_color_map(x$b, "Set3"),
#'                    shape = "all shapes")
#' aes_map(x)
aes_map <- function(x, aes_name = NULL, ...) {
  UseMethod("aes_map", x)
}

#' @export
#' @rdname aes_map
aes_map.default <- function(x, aes_name = NULL, ...) {
  out <- attr(x, "facile_aes_map")
  if (is.null(out)) out <- list()
  if (!is.null(aes_name)) {
    assert_string(aes_name)
    assert_subset(aes_name, aes_names())
    if (aes_name %in% names(out)) {
      out <- out[[aes_name]]
    } else {
      warning("`", aes_name, "` not found in aes_map")
      out <- NULL
    }
  }
  out
}


#' @export
#' @rdname aes_map
"aes_map<-" <- function(x, value, aes_name = NULL) {
  UseMethod("aes_map<-", x)
}

#' @export
#' @importFrom stats setNames
#' @rdname aes_map
"aes_map<-.default" <- function(x, value, aes_name = NULL) {
  amap <- aes_map(x)
  if (is.null(value)) {
    # resets the aes_map
    amap <- list()
  } else {
    if (!is.null(aes_name)) {
      assert_string(aes_name)
      value <- setNames(list(value), aes_name)
    }
    assert_named(value)
    assert_subset(names(value), aes_names())
    for (aname in names(value)) {
      amap <- .update.aes_map(amap, aname, value[[aname]])
    }
  }
  attr(x, "facile_aes_map") <- amap
  x
}

.update.aes_map <- function(amap, aes_name, value,
                            verbose = getOption("facileviz_verbose", FALSE)) {
  assert_class(amap, "list")
  assert_string(aes_name)
  assert_subset(aes_name, aes_names())
  if (aes_name %in% names(amap) && isTRUE(verbose)) {
    warning("Replacing aesthetic map: ", aes_name)
  }
  amap[[aes_name]] <- value
  amap
}

# with_* =======================================================================

#' Utility function used by most plotting functions that returns a data.frame
#' with columns mapping the observations to colors, shape, etc (adds columns
#' for each)
#'
#' @export
#' @examples
#' x <- data.frame(x = 1:5, y = 1:5, z = 1:5,
#'                 b = letters[1:5], c = letters[6:10],
#'                 d = letters[11:15])
#' xx <- with_aesthetics(x, color_aes = c("b", "c"), shape_aes = "d")
with_aesthetics <- function(dat, ...) {
  UseMethod("with_aesthetics", dat)
}

#' @export
with_aesthetics.data.frame <- function(dat, color_aes = NULL, color_map = NULL,
                                       shape_aes = NULL, shape_map = NULL,
                                       size_aes = NULL, size_map = NULL,
                                       hover = NULL, na_to_char = TRUE, ...) {
  amaps <- unique(c(color_aes, shape_aes, size_aes, hover))

  xx <- dat

  if (is.character(hover)) {
    hvals <- lapply(hover, function(wut) {
      vals <- xx[[wut]]
      if (is.numeric(vals)) vals <- prettyNum(round(vals, 2), big.mark = ",")
      if (!is.character(vals)) vals <- as.character(vals)
      paste0(wut, ": ", vals)
    })
    xx[[".hover"]] <- do.call(paste, c(hvals, list(sep = "<br>")))
  } else {
    xx[[".hover"]] <- ""
  }

  if (is.null(amaps)) {
    return(xx)
  }

  # Replace real NA values with "NA." so that points don't drop out in plotly
  # plots.
  if (na_to_char) {
    for (cov in amaps) {
      vals <- xx[[cov]]
      if (test_categorical(vals)) {
        isna <- is.na(vals)
        if (any(isna)) {
          if (is.factor(vals)) {
            lvls <- c(setdiff(levels(vals), c("NA", "NA.")), "NA.")
          } else {
            warning("Converting `", cov, "` character to factor for aes map")
            lvls <- setdiff(sort(unique(vals)), "NA.")
            lvls <- c(lvls, "NA.")
          }
          vals <- as.character(vals)
          vals[isna] <- "NA."
          xx[[cov]] <- factor(vals, lvls)
        }
      }
    }
  }


  assert_character(amaps)
  assert_subset(amaps, names(dat))

  xx <- with_color(xx, color_aes, color_map, ...)
  xx <- with_shape(xx, shape_aes, shape_map, ...)

  if (!is.null(size_aes)) {
    warning("We are skipping size aesthetic maps for now ...")
  }
  # xx <- with_size(xx, size_aes, size_map, ...)

  xx
}

with_aesthetics.tbl <- function(dat, color_aes = NULL, color_map = NULL,
                                shape_aes = NULL, shape_map = NULL,
                                size_aes = NULL, size_map = NULL,
                                hover = NULL, ...) {
  with_aesthetics.data.frame(collect(dat, n = Inf),
                             color_aes = color_aes, color_map = color_map,
                             shape_aes = shape_aes, shape_map = shape_map,
                             size_aes = size_aes, size_map = size_map,
                             hover = hover, ...)
}

# with_color -------------------------------------------------------------------

#' Augment a facile object with a color aesthetic
#'
#' This function augments a variety of object with color mappings. For now we
#' assume objects are only data.frames or tbls.
#'
#' @export
#' @rdname with_color
#' @seealso [aes_map()]
#'
#' @param x an object to colorize (currently just data.frame or tbls)
#' @param aesthetic an "attribute" from `x` to color. The types of object this
#'   argument can be is specific to the class of `x`. For instance, for a
#'   `data.frame`, `aesthetic` would be a character vector that specifices
#'   the column(s) used to map observations to colors.
#' @param aes_map the "logic" to use to map `aesthetic` to color. This can be
#'   something like the name of an RColorBrewer map.
#' @param ... arguments to pass down to specific implementations
#' @param .default_color the default color used when only one level exists
#' @return the object `x` with an upated `aes_map(x, "color")`
with_color <- function(x, aesthetic, aes_map = NULL, ...,
                       .default_color = I("black")) {
  UseMethod("with_color", x)
}

#' @noRd
with_color.default <- function(x, aesthetic, aes_map = NULL, ...,
                               .default_color = I("black")) {
  stop("No with_color.default method defined")
}

#' @section data.frames and tibbles:
#'
#' data.frames and tibbles are augmented with:
#'
#' 1. A `.color_aes.variable` column that hold the value of the element "level"
#'    for the color mapping
#' 2. `.color_aes.value` column that holds the actual color for the observation
#' 3. an updated aes_map(`x`, "color")` color map that has the color map
#'
#' The reason we add a `.color_aes.variable` is because the user may decide
#' to color observations by combinations of columns, not just a single column.
#'
#' @export
#' @method with_color data.frame
#' @rdname with_color
#'
#' @param out_column the name of the column that is appended to `x` that will
#'   hold the colors
#' @param ... moar things
#' @return `x` with an additional `out_column` column that holds the colors for
#'   the observations in the rows of `x`
#'
#' @examples
#' x <- data.frame(
#'   category = sample(c("a", "b", "c"), 20, replace = TRUE),
#'   subcat = sample(c("y", "z"), 20, replace = TRUE),
#'   zscore = rnorm(20),
#'   score = runif(20, min = 0, max = 1))
#' x <- with_color(x, aesthetic = "category", "Set3")
#' y <- with_color(x, aesthetic = "category", "Set1")
#' z <- with_color(x, aesthetic = "category", "viridis")
#' m <- with_color(x, aesthetic = "score")
with_color.data.frame <- function(x, aesthetic = NULL, aes_map = NULL,
                                  out_column = ".color_aes.", ...,
                                  .default_color = "black") {
  stopifnot(is(x, "data.frame") || is(x, "tbl"))
  aes_cols <- .aes_varval_colnames(out_column, x)
  if (!is.null(aesthetic) && length(aesthetic) > 0L) {
    aes_cols[["label"]] <- paste(aesthetic, collapse = "_")
    x <- .with_aes_columns(x, aesthetic, aes_cols)
    cmap <- create_color_map(x[[aes_cols$variable]], map = aes_map, ...)
    if (is.character(cmap)) {
      if (length(cmap) == 1L) {
        x[[aes_cols$value]] <- I(.default_color)
      } else {
        # x[[aes_cols$value]] <- I(cmap[x[[aes_cols$variable]]])
        cmap.idx <- x[[aes_cols$variable]]
        if (is.logical(cmap.idx)) {
          # if x[[aes_cols$variable]] is a logical covariate, the TRUE/FALSE
          # indexing will create a vector that doesn't work when indexed like
          # that so we turn it to a character first so that it's "TRUE" and
          # "FALSE", which will match the names of the color map
          cmap.idx <- as.character(cmap.idx)
        }
        x[[aes_cols$value]] <- I(unname(cmap[cmap.idx]))
      }
    } else if (is.function(cmap)) {
      # maybe aes_map was some type of colorRamp-like or viridis-likefunction,
      # then what?
      # stop("colorRamp functions not handled yet")
      x[[aes_cols$value]] <- cmap(x[[aes_cols$variable]])
    } else {
      stop("Don't know what type of color ramp we are working with here")
    }
    attr(cmap, "columns") <- aes_cols
    aes_map(x, "color") <- cmap
  }

  x
}

#' @export
#' @method with_color tbl
#' @rdname with_color
with_color.tbl <- function(x, aesthetic = NULL, aes_map = NULL,
                           out_column = ".color_aes.", ...,
                           .default_color = I("black")) {
  with_color.data.frame(collect(x), aesthetic, aes_map,
                        out_column = out_column, ...,
                        .default_color = .default_color)
}

# with_shape ===================================================================


#' Augment a facile object with a shape aesthetic
#'
#' This function augments a variety of objects with shape mappings. For now we
#' assume objects are only data.frames or tbls.
#'
#' @export
#' @rdname with_shape
#' @seealso [aes_map()]
#'
#' @param x an object to colorize (currently just data.frame or tbls)
#' @param aesthetic an "attribute" from `x` to map levels -> shapes. 
#'   The types of object this argument can be is specific to the class of `x`.
#'   For instance, for a `data.frame`, `aesthetic` would be a character vector
#'   that specifices the column(s) used to map observations to colors.
#' @param aes_map unsupported for now, no way to provide your own shapes.
#' @param ... arguments to pass down to specific implementations
#' @param .default_shape the default shape used when only one level exists, this
#'   is a `"circile"`.
#' @return the object `x` with an upated `aes_map(x, "shape")`
with_shape <- function(x, aesthetic, aes_map = NULL, ...,
                       .default_shape = I("circle")) {
  UseMethod("with_shape", x)
}

#' @export
#' @method with_shape data.frame
#' @rdname with_shape
with_shape.data.frame <- function(x, aesthetic, aes_map = NULL,
                                  out_column = ".shape_aes.", ...,
                                  .default_shape = "circle") {
  stopifnot(is(x, "data.frame") || is(x, "tbl"))
  aes_cols <- .aes_varval_colnames(out_column, x)

  if (!is.null(aesthetic) && length(aesthetic) > 0L) {
    x <- .with_aes_columns(x, aesthetic, aes_cols)
    smap <- create_shape_map(x[[aes_cols$variable]], map = aes_map, ...)
    if (is.character(smap) || is.integerish(smap)) {
      if (length(smap) == 1L) {
        x[[aes_cols$value]] <- I(.default_shape)
      } else {
        x[[aes_cols$value]] <- I(smap[x[[aes_cols$variable]]])
      }
    }
    attr(smap, "columns") <- aes_cols
    aes_map(x, "shape") <- smap
  }

  x
}

#' @export
#' @rdname with_shape
with_shape.tbl <- function(x, aesthetic, aes_map = NULL,
                           out_column = ".shape_aes.", ...,
                           .default_shape = I("circle")) {
  x <- collect(x, n = Inf)
  NextMethod()
  # with_shape.data.frame(collect(x), aesthetic, aes_map,
  #                       out_column = out_column, ...,
  #                       .default_shape = .default_shape)
}

# with_size ====================================================================

#' Augment a facile object with a size aesthetic.
#' @export
with_size <- function(x, aesthetic, aes_map = NULL,
                      out_column = ".size_aes.", ...,
                      .default_size = I(1L)) {

  UseMethod("with_size", x)
}

#' @export
#' @method with_size data.frame
#' @rdname with_size
with_size.data.frame <- function(x, aesthetic, aes_map = NULL,
                                 out_column = ".size_aes.", ...,
                                 .default_size = I(1L)) {
  if (!is.null(aesthetic)) {
    warning("We aren't mapping size")
  }
}

#' @export
#' @rdname with_size
with_size.tbl <- function(x, aesthetic, aes_map = NULL,
                          out_column = ".size_aes.", ...,
                          .default_size = I(1L)) {
  x <- collect(x, n = Inf)
  NextMethod()
}

# with_hover ===================================================================
with_hover <- function(x, aesthetic, aes_map = NULL,
                       out_column = ".hover_aes.", ...,
                       .default_hover = NULL) {
}

# with_* utilities -------------------------------------------------------------

#' Create variable and value names for an aesthethic name
#'
#' @noRd
#' @param varname the base pattern to create names for
#' @param dat (optional) data.frame to add `*.variable` and `*.value` columns to
#'   If provided, some extra checking might be done
#' @return a `list(variable = name, value = name)`
.aes_varval_colnames <- function(varname, dat = NULL, ...) {
  assert_string(varname)

  var.name <- sub("\\.*$", ".variable", varname)
  var.nane <- paste0("", ".variable")
  val.name <- sub("\\.variable$", ".value", var.name)

  if (is.data.frame(dat) || is.tbl(dat)) {
    if (var.name %in% names(dat)) {
      warning(".aes_varname column `", var.name,
              "`already exists in data.frame")
    }
    if (val.name %in% names(dat)) {
      warning(".aes_valname column `", val.name,
              "`already exists in data.frame")
    }
  }
  list(variable = var.name, value = val.name)
}

#' @noRd
#' @importFrom FacileData test_categorical
.with_aes_columns <- function(x, aesthetic, aes_cols, ...) {
  # We aren't doing tidyeval yet, so this can only be a character
  assert_data_frame(x)
  assert_character(aesthetic, min.len = 1L)
  assert_subset(aesthetic, colnames(x))
  amap <- aes_map(x)
  avar <- aes_cols$variable
  if (length(aesthetic) == 1L) {
    x[[avar]] <- x[[aesthetic]]
  } else {
    is.cat <- sapply(x[, aesthetic], test_categorical)
    assert_true(all(is.cat))
    x <- unite(x, {{avar}}, one_of(aesthetic),
               sep = "__", remove = FALSE)
  }
  aes_map(x) <- amap
  x
}
