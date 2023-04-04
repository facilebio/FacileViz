library(plotly)
library(lubridate)

econ <- economics |>
  mutate(yr = year(date), mnth = month(date))

econ |>
  group_by(yr) |>
  plot_ly(x = ~mnth, y = ~uempmed) |>
  add_lines(text = ~yr)

plot_ly(econ, x = ~mnth, y = ~uempmed) |>
  add_lines(color = ~ordered(yr))

plot_ly(econ, x = ~mnth, y = ~uempmed) |>
  add_lines(split = ~yr, color = I("black"))

plot_ly(mpg, x = ~cty, y = ~hwy, type = "scatter", mode = "markers",
        color = ~ factor(cyl))

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) |>
  add_markers(type = "scatter", color = ~ factor(cyl))

plot_ly(mpg, x = ~cty, y = ~hwy) |>
  add_markers(color = ~ factor(cyl))

plot_ly(mpg, x = ~cty, y = ~hwy) |>
  add_markers(type = "scatter", color = ~ factor(cyl), marker = list(size = 5))

