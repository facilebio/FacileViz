# Color Maps ===================================================================

#' Map values to colors.
#'
#' This is an S3 generic functions which is used to map values to colors for
#' plotting. The easiest "no brainer" way to call this would be to pass in
#' a data.frame. In this case, this will produce a color map for each of the
#' columns of a data.frame
#'
#' Map can be an RColorBrewer name, or a vector of colors. When `x` is a
#' data.frame, you can mix and match "named" palettes with custom ones, by
#' providing a names list of these, where the names of the `map` list match the
#' colnames of `x`. See the examples (`cols5`) for an example of that.
#'
#' @export
#' @importFrom FacileData test_categorical
#'
#' @param x a data.frame or vector of values to create a colormap over. Curently
#'   this is restricted to categorical vectors (character, factor), but
#'   something smart will happen when you provide a numeric vector in due time.
#' @param map a map specification. defaults to a combination of
#'   RColorBrewer Set1 and Set2 colors
#' @param sort_levels If `TRUE`, the unique levels of `vals` will be first
#'   (lexicographically) sorted before hit with colors. By default, if `vals`
#'   is a factor, the order of the levels will be kept. If `vals` is a
#'   character, they will be sorted by default.
#' @return a named list of colormaps. Maps for discrete variables are named
#'   character vectors, where `names()` are the unique levels of
#'   `vals`, and the value is the color it maps to. Colors will be recycled
#'   if there are more levels than colors provided by `map`. Colormaps for
#'   real valued variables are [circlize::colorRamp2()] functions. If `x` is a
#'   `data.frame`, a list of color maps will be returned: one for each column
#' @examples
#' dat <- data.frame(
#'   a = sample(letters[1:3], 10, replace = TRUE),
#'   b = sample(letters[4:5], 10, replace = TRUE),
#'   c = factor(sample(letters[6:8], 10, replace = TRUE)),
#'   d = rnorm(10),
#'   stringsAsFactors = FALSE)
#' cols1 <- create_color_map(dat)
#' cols2 <- create_color_map(dat, c(a = "Set2", b = "Set1", "Set3"))
#' cols3 <- create_color_map(dat, c(a = "viridis", c = "Set2", "Set3"))
#' if (interactive()) {
#'   scales::show_col(cols1$a)
#'   scales::show_col(cols2$a)
#'   scales::show_col(cols3$a)
#'   with(dplyr::arrange(dat, d), plot(d, col = cols3$d(d), pch = 16))
#' }
#' cols4 <- create_color_map(dat, c(b = "Set1", c = "Set2"))
#' cols5 <- create_color_map(
#'   dat,
#'   list(a = "Set1", b = c(d = "black", e = "grey")))
create_color_map <- function(x, map = NULL, variant = NULL, ...) {
  UseMethod("create_color_map", x)
}

#' @rdname create_color_map
#' @export
#' @method create_color_map data.frame
#' @param shuffle When `TRUE` (default), the default color map for each column
#'   will be a shuffled version of the `FacileViz:::mucho.colors()` palette. In
#'   this case, all but the first column will be shuffled. If `FALSE`, no
#'   shuffling is done, and the first, second, third, etc. colors for each
#'   discrete map will be the same.
create_color_map.data.frame <- function(x, map = NULL, variant = NULL,
                                        shuffle = TRUE, ...) {
  defaults <- lapply(seq(x), function(i) {
    if (test_categorical(x[[i]])) {
      if (i == 1L || !shuffle) mucho.colors() else sample(mucho.colors())
    } else {
      NULL
    }
  })
  names(defaults) <- colnames(x)
  if (is.null(map)) {
    map <- defaults
  } else if (is.character(map)) {
    map.in <- map
    map <- list()
    mapped <- intersect(names(map.in), colnames(x))
    if (length(mapped)) {
      map[mapped] <- map.in[mapped]
    }
    add.me <- setdiff(colnames(x), names(map))
    no.name <- names(map.in) == ""
    if (any(no.name)) {
      nms <- head(add.me, length(no.name))
      map[nms] <- map.in[no.name]
    }
  }
  assert_list(map)
  add.me <- setdiff(colnames(x), names(map))
  for (name in add.me) {
    map[[name]] <- defaults[[name]]
  }
  sapply(colnames(x), function(name) {
    create_color_map(x[[name]], map[[name]], ...)
  }, simplify = FALSE)
}

#' @noRd
#' @export
create_color_map.character <- function(x, map = NULL, variant = NULL,
                                       sort_levels = TRUE, ...) {
  .create_color_map.categorical(x, map = map, variant = variant,
                                sort_levels = sort_levels, ...)
}

#' @noRd
#' @export
create_color_map.factor <- function(x, map = NULL, variant = NULL,
                                    sort_levels = FALSE, ...) {
  .create_color_map.categorical(x, map = map, variant = variant,
                                sort_levels = sort_levels, ...)
}

#' @noRd
#' @export
create_color_map.logical <- function(x, map = NULL, variant = NULL,
                                     sort_levels = FALSE, ...) {
  create_color_map(as.character(x), map = map, variant = variant,
                   sort_levels = sort_levels, ...)
}

#' Internal method that handles both chracter and factor vectors
#' because create_color_map.atomic doesn't work
#'
#' @noRd
#' @param sort_levels If `TRUE`, the unique levels of `vals` will be first
#'   (lexicographically) sorted before hit with colors. By default, if `vals`
#'   is a factor, the order of the levels will be kept. If `vals` is a
#'   character, they will be sorted by default.
#' @return a character vector like `map` but with recycled entries if the number
#'   of `length(unique(vals)) > length(map)`
.create_color_map.categorical <- function(x, map = NULL, variant = NULL,
                                          sort_levels = !is.factor(x), ...) {
  assert_categorical(x)
  if (is.factor(x)) {
    uvals <- levels(x)
  } else {
    uvals <- unique(as.character(x))
  }

  if (is.null(map)) {
    map <- mucho.colors()
  } else if (test_string(map) && map == "viridis") {
    map <- scales::viridis_pal()(length(uvals))
  } else if (is.brewer.map.name(map)) {
    map <- suppressWarnings(brewer.pal(20, map))
  }

  if (!is.character(map)) {
    stop("The color map should be a vector of characters by now")
  }

  # we used to allow an "integerish" map vector, but not sure what that was
  # at the moment ... and the is.integerish method has gone missing (2019-04-01)
  map.type <- if (is.character(map)) "char" else "int"

  if (sort_levels) {
    uvals <- sort(uvals)
  }

  if (is.null(names(map))) {
    out.map <- if (map.type == "char") character() else integer()
    rest.map <- map
  } else {
    out.map <- map[names(map) %in% uvals]
    rest.map <- unname(map[!names(map) %in% names(out.map)])
  }

  remain <- setdiff(uvals, names(out.map))
  if (length(remain)) {
    cols <- unname(c(rest.map, out.map))
    idxs <- seq(remain) %% length(cols)
    idxs[idxs == 0] <- length(cols)
    rest.map <- cols[idxs]
    names(rest.map) <- remain
    out.map <- c(out.map, rest.map)
  }

  out.map
}

#' This was code extracted from the multiGSEA::mgheatmap logic that guesses
#' the type of color to use for a heatmap. If `x` looks like 0-centered data,
#' we will use a divergent color map (blue-white-red). If it looks like an
#' abundance measure, we will use viridis.
#'
#' In the future, the `map` parameter will be used to define the type of colors
#' used such that diverging maps can be something else besides blue->white->red,
#' or real valued "abundance" can be something other than default viridis.
#'
#' @noRd
#' @importFrom circlize colorRamp2
create_color_map.numeric <- function(x, map = NULL, zlim = NULL, ...) {
  if (!is.null(map)) {
    warning("map parameter is curently ignored for numeric color maps, ",
            "only viridis is used")
  }
  # Is 0 close to the center of the score distribution?
  mean.x <- mean(x)
  zero.center <- mean.x >= -0.2 && mean.x <= 0.2
  if (zero.center) {
    if (missing(zlim)) {
      fpost <- quantile(abs(x), 0.975)
      zlim <- c(-fpost, fpost)
    } else if (is.null(zlim)) {
      zlim <- c(min(x), max(x))
    } else {
      stopifnot(zlim[1L] < 0, zlim[2L] > 0)
    }
    col <- colorRamp2(
      c(zlim[1L], 0, zlim[2L]),
      c('#1F294E', 'white', '#6E0F11'))
  } else {
    if (missing(zlim)) {
      fpost <- quantile(x, c(0.025, 0.975))
    } else if (is.null(zlim)) {
      fpost <- c(min(x), max(x))
    } else {
      stopifnot(all(zlim >= 0), all(zlim <= 1))
      fpost <- quantile(x, zlim)
    }
    breaks <- quantile(x, seq(0, 1, by = 0.05))
    if (fpost[1L] > breaks[2L] || fpost[2L] < breaks[20L]) {
      stop("Illegal values for zlim")
    }
    breaks[1] <- fpost[1]
    breaks[21] <- fpost[2]
    col <- colorRamp2(breaks, scales::viridis_pal()(21))
  }
  col
}

# Shape Maps ===================================================================

#' Maps shapes to categorical values
#'
#' Map unique leves of `vals` to different shapes. Only works for categorical
#' variables.
#'
#' TODO: Use plotly shapes (currently we use base R pch). This webpage shows
#' you the symbols and how to generate them:
#' http://www.r-graph-gallery.com/125-the-plotlys-symbols/
#'
#' @export
#' @importFrom FacileData assert_categorical
#' @param vals a vector of categorical values
#' @param map a map definition. By default we use pch symbol identifiers.
#' @return a named vector. `names()` are the unique values in `vals`, and values
#'   are the different shapes (pch integers)
#' @examples
#' # This isn't a real example. It is the code from the aforementioned page
#' # that generates the plotly shapes.
#' library(plotly)
#' data=expand.grid(c(1:6) , c(1:6))
#' data=cbind(data , my_symbol=c(1:36))
#' data=data[data$my_symbol<33 , ]
#'
#' # Make the graph
#' my_graph=plot_ly(data , x=~Var1 , y=~Var2 , type="scatter",
#'                  mode="markers+text" , hoverinfo="text", text=~my_symbol,
#'                  textposition = "bottom right",
#'                  marker=list(symbol=~my_symbol, size=40, color="red",
#'                              opacity=0.7)) %>%
#'   layout(
#'     hovermode="closest",
#'     yaxis=list(autorange="reversed", title="",
#'                tickfont=list(color="white")) ,
#'     xaxis=list( title="" , tickfont=list(color="white"))
#'   )
#' # show graph
#' my_graph
create_shape_map <- function(vals, map = NULL, ..., is3d = FALSE) {
  assert_categorical(vals)

  # These work for 2d plots, but 3d plots don't support all the shapes.
  # https://plot.ly/r/reference/#scatter3d-marker-symbol
  #
  # And these are maybe too many shapes to be distinguishing by eye anyway,
  # let's generate a shape map that is compatible w/ 2d and 3d plots.
  # # plotly symbols go from 1:32. I rearrange them here a bit to put the most
  # # visually diverse ones up front
  # if (is.null(map)) {
  #   all.shapes <- 1:32
  #   # remove ones that look too similar
  #   shapes <- setdiff(all.shapes, c(14:16, 28, 20, 32))
  #   first <- c(27, 3, 17, 1, 2, 13)
  #   map <- c(first, setdiff(shapes, first))
  # }
  if (!is.null(map)) {
    warning("We don't support custom shape maps  yet", immediate. = TRUE)
  }
  safe.map <- c(
    "circle",      "square",      "diamond", "cross", "x",
    "circle-open", "square-open", "diamond-open")
  if (is3d || is.null(map) || !is.character()) {
    map <- safe.map
  } else {
    map <- safe.map
  }
  out <- xref.discrete.map.to.vals(map, vals)
  out
}

#' @noRd
#' @param map named character vector, where names are the entries found in
#'   `vals`
#' @param vals a categorical vector (character or factor)
#' @param sort_levels If `TRUE`, the unique levels of `vals` will be first
#'   (lexicographically) sorted before hit with colors. By default, if `vals`
#'   is a factor, the order of the levels will be kept. If `vals` is a
#'   character, they will be sorted by default.
#' @return a character vector like `map` but with recycled entries if the number
#'   of `length(unique(vals)) > length(map)`
xref.discrete.map.to.vals <- function(map, vals,
                                      sort_levels = !is.factor(vals)) {
  assert_categorical(vals)
  stopifnot(is.character(map) || is.integerish(map))
  map.type <- if (is.character(map)) "char" else "int"

  if (is.factor(vals)) {
    uvals <- levels(vals)
  } else {
    uvals <- unique(as.character(vals))
  }

  if (sort_levels) {
    uvals <- sort(uvals)
  }

  if (is.null(names(map))) {
    out.map <- if (map.type == "char") character() else integer()
    rest.map <- map
  } else {
    out.map <- map[names(map) %in% uvals]
    rest.map <- unname(map[!names(map) %in% names(out.map)])
  }

  remain <- setdiff(uvals, names(out.map))
  if (length(remain)) {
    cols <- unname(c(rest.map, out.map))
    idxs <- seq(remain) %% length(cols)
    idxs[idxs == 0] <- length(cols)
    rest.map <- cols[idxs]
    names(rest.map) <- remain
    out.map <- c(out.map, rest.map)
  }

  out.map
}

#' @noRd
#' @importFrom RColorBrewer brewer.pal.info
is.brewer.map.name <- function(x) {
  test_string(x) && test_choice(x, rownames(brewer.pal.info))
}

#' @noRd
#' @importFrom RColorBrewer brewer.pal
mucho.colors <- function() {
  s1 <- RColorBrewer::brewer.pal(9, "Set1")
  s2 <- RColorBrewer::brewer.pal(8, "Set2")
  s3 <- RColorBrewer::brewer.pal(12, "Set3")

  # the sixth set1 color is a yellow that is too bright for anyone's good
  muchos <- c(s1[-6], s2[1:8])

  # Now the 5th and 10th are both a very similar orange, remove 10
  muchos <- muchos[-10]

  # Let's put grey orange up front, positions 8 and 5, respecitvely
  out <- c(muchos[8], muchos[5], muchos[-c(8,5)])
  # and make cornflower blue third (and the 4th position (a blue), is red )
  out[4] <- out[3]
  out[3] <- "#6495ED" # cornflower blue
  out
}

