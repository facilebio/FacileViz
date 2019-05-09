library(plotly)
theme_set(theme_bw())

dat <- data.frame(
  variety = rep(LETTERS[1:7], each = 40),
  treatment = sample(c("high", "med", "low"), 7 * 40, replace = TRUE),
  note = seq(7 * 40) + sample(1:150, 7 * 40, replace = TRUE))

# Dueling banjos

## Ungrouped
ggplot(dat, aes(x = variety, y = note)) +
  geom_boxplot()

plot_ly(dat, x = ~ variety, y = ~ note) %>%
  add_boxplot(boxpoints = "outliers", fillcolor = "white", pointpos = 0)

fboxplot(dat, x = "variety", y = "note")

# One boxplot per x-axis, different colored points
gg <- ggplot(dat, aes(x = variety, y = note)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = treatment), width = 0.1)
gg

# need to ensure x is a factor so you can convert to numeric for point jitter
plot_ly(dat, x = ~as.numeric(variety), y = ~note) %>%
  add_boxplot(boxpoints = FALSE, showlegend = FALSE, fillcolor = "white") %>%
  add_markers(x = ~jitter(as.numeric(variety), amount = 0.07),
              color = ~treatment) %>%
  layout(xaxis = list())

fboxplot(dat, x = "variety", y = "note", color_aes = "treatment")
fboxplot(dat, x = "variety", y = "note", color_aes = "treatment")
ggplot(dat, aes(x = variety, y = note)) +
  geom_boxplot() +
  facet_wrap(~ treatment)
fboxplot(dat, x = "variety", y = "note", facet_aes = "treatment")

## Grouped
gg <- ggplot(dat, aes(x=variety, y=note, fill=treatment)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge())
gg

plot_ly(dat, x = ~variety, y = ~note) %>%
  # add_boxplot(boxpoints = FALSE, showlegend = FALSE, fillcolor = "white") %>%
  add_boxplot(boxpoints = "all", color = ~ treatment, pointpos = 0) %>%
  layout(boxmode = "group")


# From rbook
plot_ly(diamonds, x = ~interaction(clarity, cut), y = ~price) %>%
  add_boxplot(color = ~clarity) %>%
  layout(yaxis = list(title = ""))

ggplot(diamonds, aes(cut, price, fill = clarity)) +
  geom_boxplot()


dat <- sample_n(diamonds, 2000)
plot_ly(dat, x = ~cut, y = ~price) %>%
  add_boxplot(color = ~clarity) %>%
  layout(boxmode = "group")

plots <- dat %>%
  group_by(cut) %>%
  do(plot = {
    plot_ly(., x = ~as.numeric(clarity), y = ~price, legendgroup = ~clarity,
            showlegend = .$cut[1] == diamonds$cut[1]) %>%
      add_boxplot(pointpos = 0,
                  boxpoints = FALSE,
                  # color = ~ clarity,
                  line = list(color = "black"),
                  fillcolor = "white",
                  showlegend = FALSE) %>%
      add_markers(x = ~jitter(as.numeric(clarity)), y = ~price,
                  color = ~ clarity) %>%
      layout(xaxis = list(tickvals = 1:8, ticktext = levels(diamonds$clarity)))
  })
subplot(plots, nrows = 1, shareY = TRUE)
