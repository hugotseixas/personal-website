
library(tidytuesdayR)
library(magrittr)
library(lubridate)
library(MetBrewer)
library(showtext)
library(glue)
library(janitor)
library(cowplot)
library(patchwork)
library(ggforce)
library(tidyverse)

tuesdata <- tt_load(2020, week = 36) # Load data from tidy Tuesday

key_crop_yields <- tuesdata$key_crop_yields %>%
  pivot_longer(4:14, names_to = "crop", values_to = "yield") %>%
  clean_names() %>%
  drop_na() %>%
  group_by(entity, crop) %>%
  mutate(
    crop = str_remove(crop, pattern = coll(" (tonnes per hectare)" )),
    relative_yield = (yield - first(yield)) / first(yield) * 100
  ) %>%
  filter(
    crop %in% c("Beans", "Maize", "Potatoes", "Rice", "Soybeans", "Wheat")
  )
  
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application %>%
  rename(yield = 4, nitrogen = 5) %>%
  clean_names() %>%
  drop_na()

land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production %>%
  rename(Yield = 4, Area = 5, population = 6) %>%
  drop_na() %>%
  pivot_longer(c(4, 5)) %>%
  clean_names() %>%
  mutate(year = as.numeric(year))

tractor <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture %>%
  rename(tractor = 4, yield = 5, population = 6) %>%
  clean_names() %>%
  drop_na()

font_add_google("Josefin Sans")
font_add_google("Abel")
font_add_google("Signika Negative")

showtext_auto()

p1 <- key_crop_yields %>%
  filter(entity == "Brazil") %>%
  ggplot() +
  geom_point(aes(x = year, y = relative_yield, color = crop)) +
  geom_smooth(
    aes(x = year, y = relative_yield, color = crop),
    se = FALSE
  ) +
  scale_color_met_d("Peru2") +
  labs(
    x = NULL, y = "Yield Percent Change (%)",
    title = "Percent change of yield of key crops in Brazil"
  ) +
  lims(x = c(1960, 2018)) +
  theme_cowplot(font_family = "Abel") +
  theme(legend.title = element_blank())

p2 <- land_use %>% 
  filter(code %in% c("BRA")) %>%
  ggplot() +
  geom_line(
    aes(x = year, y = value, linetype = name, group = name)
  ) +
  labs(
    x = "Year", y = "Relative Change (%)",
    title = "Relative change of yield and agricultural areas in Brazil"
  ) +
  lims(x = c(1960, 2018)) +
  annotate(
    "text", x = 1985, y = 330, 
    size = 2.5,
    label = "Yield growth exceeded \n area expansion",
    family = "Signika Negative"
  ) +
  geom_curve(
    aes(x = 1991, y = 310, xend = 1993, yend = 200),
    arrow = arrow(length = unit(0.08, "inch")), 
    linewidth = 0.3,
    curvature = -0.2,
    lineend = "round"
  ) +
  theme_cowplot(font_family = "Abel") +
  theme(legend.title = element_blank())

p3 <- fertilizer %>%
  filter(code %in% c("BRA", "USA")) %>%
  ggplot(aes(x = yield, y = nitrogen, shape = code)) +
  geom_point() +
  stat_ellipse() +
  annotate(
    "text", x = 6, y = 20, 
    size = 2.5,
    label = "Less efficient \n use of nitrogen",
    family = "Signika Negative"
  ) +
  geom_curve(
    aes(x = 4.5, y = 24, xend = 3.8, yend = 31),
    arrow = arrow(length = unit(0.08, "inch")), 
    linewidth = 0.3,
    curvature = -0.2,
    lineend = "round"
  ) +
  annotate(
    "text", x = 7, y = 48, 
    size = 2.5,
    label = "Yield growth with small \n  nitrogen increase",
    family = "Signika Negative"
  ) +
  geom_curve(
    aes(x = 8, y = 53, xend = 7.8, yend = 62),
    arrow = arrow(length = unit(0.08, "inch")), 
    linewidth = 0.3,
    curvature = 0.2,
    lineend = "round"
  ) +
  labs(
    x = "Cereal Yield (tonnes/ha)", y = "Nitrogen Fertilizer (kg/ha)",
    title = "Relation between yield, fertilization and tractor use"
  ) +
  theme_cowplot(font_family = "Abel") +
  theme()

p4 <- tractor %>%
  filter(code %in% c("BRA", "USA")) %>%
  ggplot(aes(x = yield / 1000, y = tractor, shape = code)) +
  geom_point() +
  geom_mark_circle() +
  labs(
    x = "Cereal yield (tonnes/ha)",
    y = "Tractors (unit/100km²)",
  ) +
  annotate(
    "text", x = 3, y = 69, 
    size = 2.5,
    label = "Direction \n change",
    family = "Signika Negative"
  ) +
  annotate(
    "text", x = 4.5, y = 200, 
    size = 2.5,
    label = "Stable amount \n of tractors",
    family = "Signika Negative"
  ) +
  theme_cowplot(font_family = "Abel") +
  theme()

subp <- 
  ((p3 + p4) + 
     plot_layout(guides = "collect") & theme(legend.title = element_blank()))

p <- p1 / p2 / subp +
  plot_annotation(
    title = "Historical crop yield in Brazil",
    caption = "Hugo Tameirão Seixas | Data: Our World in Data",
    theme = theme(
      plot.title = element_text(size = 25, family = "Josefin Sans"),
      plot.caption = element_text(size = 10, family = "Josefin Sans"),
      plot.background = element_rect(fill  = '#6c8f73')
    )
  ) +
  plot_layout(heights = c(1.3, 1, 1.1))

ggsave(
  "dataviz/figures/crop_yield.svg", 
  p,
  width = 18,
  height = 24,
  units = "cm"
)

