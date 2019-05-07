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
