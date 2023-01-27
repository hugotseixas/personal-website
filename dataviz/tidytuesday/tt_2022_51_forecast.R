
library(tidytuesdayR)
library(magrittr)
library(lubridate)
library(MetBrewer)
library(showtext)
library(glue)
library(tidyverse)

tuesdata <- tt_load(2022, week = 51) # Load data from tidy Tuesday

weather_forecasts <- tuesdata$weather_forecasts
cities <- tuesdata$cities
outlook_meanings <- tuesdata$outlook_meanings

glimpse(weather_forecasts)

weather_forecasts %<>%
  filter(
    forecast_hours_before == 12,
    possible_error == "none"
  ) %>%
  select(-forecast_hours_before) %>%
  mutate(
    across(.cols = c(observed_temp:forecast_temp), ~ (.x - 32) * 0.5556),
    error_temp = observed_temp - forecast_temp,
    date = floor_date(date, unit = "month")
  )

font_add_google("Bitter")

showtext_auto()

metrics <- weather_forecasts %>%
  group_by(high_or_low) %>%
  drop_na() %>%
  summarise(
    rmse = sqrt(mean((observed_temp - forecast_temp)^2)),
    mae = mean(abs(observed_temp - forecast_temp))
  )

set.seed(2138)

p <- weather_forecasts %>%
  drop_na() %>%
  slice_sample(prop = 0.02) %>%
  ggplot() +
  facet_wrap(
    ~ high_or_low, 
    labeller = labeller(high_or_low = c("high" = "Maximum", "low" = "Minimum"))
  ) +
  stat_density_2d(
    aes(x = observed_temp, y = forecast_temp, fill = after_stat(level)),
    geom = "polygon"
  ) +
  geom_label(
    data = metrics,
    label.padding = unit(0.5, "lines"),
    label.r = unit(0, "lines"),
    label.size = 0.5,
    fill = "#ded1d1",
    hjust = 0,
    fontface = "bold",
    family = "Bitter",
    size = 4.5,
    aes(
      x = -15,
      y = 30,
      label = glue(
        "MAE = {round(mae, 2)}",
        "\n",
        "RMSE = {round(rmse, 2)}"
      )
    )
  ) +
  geom_point(
    data = . %>% slice_sample(prop = 0.05),
    aes(x = observed_temp, y = forecast_temp), 
    alpha = 0.5,
  ) +
  scale_fill_met_c(name = "Hokusai1") +
  scale_x_continuous(breaks = scales::pretty_breaks(3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(3)) +
  labs(
    title = "Temperature Forecasting Accuracy",
    caption = "Hugo Tameirão Seixas | Data: Weather Forecast Capstone Project",
    x = "Observed Temperature (ºC)", y = "Predicted Temperature (ºC)"
  ) +
  guides(fill = "none") +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#282a30", color = "transparent"),
    plot.title = element_text(
      color = "#ab7474", 
      size = 25, 
      family = "Bitter",
      hjust = -1.4
    ),
    axis.title = element_text(color = "#ded1d1", size = 15, family = "Bitter"),
    axis.text = element_text(color = "#f5e9e9", size = 12, family = "Bitter"),
    strip.text = element_text(color = "#ded1d1", size = 15, family = "Bitter"),
    plot.caption = element_text(color = "#ab7474", size = 12, family = "Bitter")
  )

ggsave(
  "dataviz/figures/temp_accuracy.svg", 
  p,
  width = 17,
  height = 11.6,
  units = "cm"
)

