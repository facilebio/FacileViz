# Helper functions to create categorical maps for shapes and colors

#' Maps colors to categorical values
#'
#' Map can be a RColorBrewer name, or a vector of colors. Colors will be
#' recycled if `length(map) <`
#' If `vals`, map can be is categoricasl
#'
#' @export
#' @param vals a vector of values to create a colormap over. Curently this is
#'   restricted to categorical vectors (character, factor), but something smart
#'   will happen when you provide a numeric vector in due time.
#' @param map a map specification. defaults to a combination of
#'   RColorBrewer Set1 and Set2 colors
#' @return a named character vector, where `names()` are the unique levels of
#'   `vals`, and the value is the color it maps to. Colors will be recycled
#'   if there are more levels than colors provided by `map`.
create_color_map <- function(vals, map = NULL) {
  is.cat <- test_categorical(vals)

  if (is.cat) {
    if (is.null(map)) map <- mucho.colors()
    if (is.brewer.map.name(map)) {
      map <- suppressWarnings(brewer.pal(20, map))
    }
    if (!is.character(map)) {
      stop("The color map should be a list of colors by now")
    }
    out <- xref.discrete.map.to.vals(map, vals)
  } else {
    stop("Not mapping real values yet")
  }
  out
}

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
create_shape_map <- function(vals, map = NULL) {
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

  map <- c(
    "circle",      "square",      "diamond", "cross", "x",
    "circle-open", "square-open", "diamond-open")
  out <- xref.discrete.map.to.vals(map, vals)
  out
}

#' @noRd
#' @importFrom RColorBrewer brewer.pal.info
is.brewer.map.name <- function(x) {
  is.character(x) && length(x) == 1L && x %in% rownames(brewer.pal.info)
}

#' @noRd
#' @importFrom RColorBrewer brewer.pal
mucho.colors <- function() {
  s1 <- RColorBrewer::brewer.pal(9, "Set1")
  s2 <- RColorBrewer::brewer.pal(8, "Set2")
  s3 <- RColorBrewer::brewer.pal(12, "Set3")

  # the sixth set1 color is a yellow that is too bright for anyone's good
  muchos <- c(s1[-6], s2[1:8])
}

#' @noRd
#' @param map named character vector, where names are the entries found in
#'   `vals`
#' @param vals a categorical vector (character or factor)
#' @return a character vector like `map` but with recycled entries if the number
#'   of `length(unique(vals)) > length(map)`
xref.discrete.map.to.vals <- function(map, vals) {
  assert_categorical(vals)
  stopifnot(is.character(map) || is.integerish(map))
  map.type <- if (is.character(map)) "char" else "int"

  if (is.factor(vals)) {
    uvals <- levels(vals)
  } else {
    uvals <- sort(unique(as.character(vals)))
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
