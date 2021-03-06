% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_aes_map.R
\name{create_color_map}
\alias{create_color_map}
\alias{create_color_map.data.frame}
\title{Map values to colors.}
\usage{
create_color_map(x, map = NULL, variant = NULL, ...)

\method{create_color_map}{data.frame}(x, map = NULL, variant = NULL, shuffle = TRUE, ...)
}
\arguments{
\item{x}{a data.frame or vector of values to create a colormap over. Curently
this is restricted to categorical vectors (character, factor), but
something smart will happen when you provide a numeric vector in due time.}

\item{map}{a map specification. defaults to a combination of
RColorBrewer Set1 and Set2 colors}

\item{shuffle}{When \code{TRUE} (default), the default color map for each column
will be a shuffled version of the \code{FacileViz:::mucho.colors()} palette. In
this case, all but the first column will be shuffled. If \code{FALSE}, no
shuffling is done, and the first, second, third, etc. colors for each
discrete map will be the same.}

\item{sort_levels}{If \code{TRUE}, the unique levels of \code{vals} will be first
(lexicographically) sorted before hit with colors. By default, if \code{vals}
is a factor, the order of the levels will be kept. If \code{vals} is a
character, they will be sorted by default.}
}
\value{
a named list of colormaps. Maps for discrete variables are named
character vectors, where \code{names()} are the unique levels of
\code{vals}, and the value is the color it maps to. Colors will be recycled
if there are more levels than colors provided by \code{map}. Colormaps for
real valued variables are \code{\link[circlize:colorRamp2]{circlize::colorRamp2()}} functions. If \code{x} is a
\code{data.frame}, a list of color maps will be returned: one for each column
}
\description{
This is an S3 generic functions which is used to map values to colors for
plotting. The easiest "no brainer" way to call this would be to pass in
a data.frame. In this case, this will produce a color map for each of the
columns of a data.frame
}
\details{
Map can be an RColorBrewer name, or a vector of colors. When \code{x} is a
data.frame, you can mix and match "named" palettes with custom ones, by
providing a names list of these, where the names of the \code{map} list match the
colnames of \code{x}. See the examples (\code{cols5}) for an example of that.
}
\examples{
dat <- data.frame(
  a = sample(letters[1:3], 10, replace = TRUE),
  b = sample(letters[4:5], 10, replace = TRUE),
  c = factor(sample(letters[6:8], 10, replace = TRUE)),
  d = rnorm(10),
  stringsAsFactors = FALSE)
cols1 <- create_color_map(dat)
cols2 <- create_color_map(dat, c(a = "Set2", b = "Set1", "Set3"))
cols3 <- create_color_map(dat, c(a = "viridis", c = "Set2", "Set3"))
if (interactive()) {
  scales::show_col(cols1$a)
  scales::show_col(cols2$a)
  scales::show_col(cols3$a)
  with(dplyr::arrange(dat, d), plot(d, col = cols3$d(d), pch = 16))
}
cols4 <- create_color_map(dat, c(b = "Set1", c = "Set2"))
cols5 <- create_color_map(
  dat,
  list(a = "Set1", b = c(d = "black", e = "grey")))
}
