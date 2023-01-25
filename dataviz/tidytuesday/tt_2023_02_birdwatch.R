
library(tidytuesdayR)
library(janitor)
library(magrittr)
library(lubridate)
library(glue)
library(tidyverse)

tuesdata <- tt_load(2023, week = 02) # Load data from tidy Tuesday

birds_data <- tuesdata$PFW_2021_public

site_data <- tuesdata$PFW_count_site_data_public_2021

glimpse(birds_data)

birds_data %<>% 
  clean_names() %>%
  filter(
    valid == 1, 
    str_detect(subnational1_code, "US")
  ) %>%
  select(
    loc_id:subnational1_code, 
    sub_id:year, 
    species_code:how_many
  ) %>%
  mutate(year_mon = ymd(glue("{year}-{month}-01"), truncated = 2))

glimpse(site_data)

site_data %<>%
  filter(
    loc_id %in% pull(birds_data, loc_id)
  )

birds_data %>%
  ggplot() +
  stat_summary(
    aes(x = year_mon, y = how_many),
    fun = "sum",
    geom = "bar",
    color = "#404040",
    linewidth = 1,
    fill = "transparent"
  ) +
  labs(x = "Month", y = NULL) +
  theme_void() +
  theme(
    axis.title.x = element_text(size = 20)
  )
  
