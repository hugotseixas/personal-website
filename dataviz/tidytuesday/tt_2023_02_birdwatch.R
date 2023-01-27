
library(tidytuesdayR)
library(sf)
library(janitor)
library(magrittr)
library(lubridate)
library(glue)
library(MetBrewer)
library(cowplot)
library(showtext)
library(geomtextpath)
library(tidyverse)

tuesdata <- tt_load(2023, week = 02) # Load data from tidy Tuesday

birds_data <- tuesdata$PFW_2021_public

birds_data %<>% 
  clean_names() %>%
  filter(
    valid == 1, 
    str_detect(subnational1_code, "US")
  ) %>%
  select(
    loc_id:subnational1_code, 
    proj_period_id,
    sub_id:year, 
    species_code:how_many
  ) %>%
  mutate(
    date = ymd(glue("{year}-{month}-{day}")),
    weekday = wday(date, label = TRUE, abbr = TRUE)
  )

site_data <- tuesdata$PFW_count_site_data_public_2021

site_data %<>%
  filter(
    loc_id %in% pull(birds_data, loc_id)
  )

glimpse(birds_data)
glimpse(site_data)

font_add_google("Anton")
font_add_google("Cabin")

showtext_auto()

palet <- "Pissaro"

p <- birds_data %>%
  group_by(weekday, date) %>%
  summarise(how_many = sum(how_many)) %>%
  mutate(
    date = factor(
      floor_date(date, unit = "month"),
      labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
    )
  ) %>%
  ggplot(
    aes(
      x = weekday,
      y = date,
      size = how_many / 1000,
      fill = date,
      group = date
    )
  ) +
  geom_hline(yintercept = 1:6, colour = "#FFFFFF", linewidth = 2, alpha = 0.3) +
  geom_jitter(
    color = "#384040", 
    alpha = 0.7, 
    shape = 21,
    position = position_jitter(height = 0.3, width = 0.3)
  ) +
  annotate(
    geom = "text",
    x = c(3.4, 3.45, 3.5, 3.55, 3.6, 3.65), 
    y = 1:6, 
    label = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
    size = 4,
    family = "Cabin"
  ) +
  coord_curvedpolar() +
  scale_y_discrete(expand = c(0, 1.4)) +
  scale_size(
    range = c(0, 8), 
    name = "Bird counts (K)", 
    breaks = c(1, 3, 5, 7)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +
  scale_fill_met_d(name = palet) +
  labs(
    x = "Month", y = NULL,
    fill = "Month", 
    title = "Sightings of birds in the U.S.",
    caption = "Hugo Tameirão Seixas | Data: Project Feeder Watch"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 15, vjust = 2, family = "Cabin"),
    plot.background = element_rect(fill = "#596663", color = "transparent"),
    plot.caption.position = "plot",
    plot.title = element_text(
      family = "Anton", 
      size = 30, 
      hjust = 0.2,
      margin = margin(b = -30)
    ),
    plot.caption = element_text(
      family = "Cabin", 
      size = 12, 
      hjust = 0.97,
      margin = margin(t = -20)
    ),
    legend.margin = margin(r = 4, l = -22),
    legend.text = element_text(family = "Cabin", size = 11),
    legend.title = element_text(family = "Cabin", size = 13)
  )

ggsave(
  "dataviz/figures/bird_count.svg", 
  p,
  width = 16,
  height = 14,
  units = "cm"
)

