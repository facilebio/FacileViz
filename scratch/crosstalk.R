# Two data.frames --------------------------------------------------------------
# https://stackoverflow.com/questions/48581598/filter-two-tables-with-crosstalk

library(plotly)
library(crosstalk)
library(tidyverse)

df1 <- structure(
  list(owner = structure(c(1L, 2L, 2L, 2L, 2L),
                         .Label = c("John", "Mark"),
                         class = "factor"),
       hp = c(250, 120, 250, 100, 110),
       car = structure(c(2L, 2L, 2L, 1L, 1L),
                       .Label = c("benz", "bmw"),
                       class = "factor"),
       id = structure(1:5,
                      .Label = c("car1", "car2", "car3", "car4", "car5"),
                      class = "factor")),
  .Names = c("owner", "hp", "car", "id"),
  row.names = c(NA, -5L),
  class = "data.frame")

df2 <- structure(
  list(car = structure(c(1L, 2L, 1L, 2L),
                       .Label = c("benz", "bmw"),
                       class = "factor"),
       owner = structure(c(1L, 1L, 2L, 2L),
                         .Label = c("John", "Mark"),
                         class = "factor"),
       freq = structure(c(0L, 1L, 2L, 2L))),
  .Names = c("car", "owner", "freq"),
  row.names = c(NA, - 4L),
  class = "data.frame")

# Filters
shared_df1 <- SharedData$new(df1, ~owner, group = "Choose owner")
shared_df2 <- SharedData$new(df2, ~owner, group = "Choose owner")

df = filter_select("owner", "Car owner:", shared_df1, ~owner)

plot_ly(shared_df1, x = ~id, y = ~hp, color = ~owner) |>
  add_markers() |>
  highlight("plotly_click")

