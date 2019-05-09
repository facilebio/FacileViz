library(dplyr)
library(plotly)

dat <- tibble(
  x = 1:6,
  y = 1:6,
  a = c("a", "a", "b", "c", "b", "d"),
  b = c("x", "y", "y", "y", "z", "z"))

colors <- c(a = "blue", b = "red", c = "green", d = "orange")

plot_ly(dat, x = ~x, y = ~y) %>%
  add_markers(type = "scatter", color = ~a, colors = colors)

plots <- sapply(unique(dat$b), function(val) {
  xdat <- filter(dat, b == val)
  plot_ly(xdat, x = ~x, y = ~y, legendgroup = ~a, showlegend = val == "y") %>%
    add_markers(type = "scatter", color = ~a, colors = colors)
}, simplify = FALSE)


sb <- subplot(plots)
sbb <- plotly_build(sb)

grps <- lapply(sbb$x$data, '[[', "legendgroup")
showit <- !duplicated(unlist(grps))
for (i in seq(sbb$x$data)) {
  sbb$x$data[[i]]$showlegend <- showit[i]
}

sbb$x$data <- lapply()
gg <- ggplot(dat, aes(x, y)) +
  geom_point(aes(color = a)) +
  facet_wrap(~ b)
ggplotly(gg)


library(dplyr)
library(plotly)
set.seed(123)

dat <- data.frame(
  a = rnorm(100), b = rnorm(100), c = rnorm(100),
  class = sample(c("g1", "g2", "g3"), 100, replace = TRUE),
  grp = sample(c("n", "o", "p", "q", "r"), 100, replace = TRUE))

axopts <- list(linecolor = toRGB("black"), linewidth = 2, showline = TRUE)
xaxopts <- list(linecolor = toRGB("black"), linewidth = 2, showline = TRUE,
                range = range(dat$a))
yaxopts <- list(linecolor = toRGB("black"), linewidth = 2, showline = TRUE,
                range = range(dat$b))
plots <- dat %>%
  group_by(class) %>%
  do(plot = {
    plot_ly(., x = ~a, y = ~b, legendgroup = ~grp,
            showlegend = .$class[1] == "g1") %>%
      add_markers(color = ~grp) %>%
      layout(xaxis = axopts, yaxis = axopts)
  })
subplot(plots, nrows = 2, shareX = TRUE, shareY = TRUE)
subplot(plots, nrows = 2, shareX = FALSE, shareY = FALSE)

library(cowplot)
ggplot(dat, aes(a, b)) +
  geom_point(aes(color = grp)) +
  facet_wrap(~ class, nrow = 2) +
  theme_classic()
