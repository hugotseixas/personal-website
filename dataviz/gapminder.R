
library(ragg)
library(gapminder)
library(scales)
library(MetBrewer)
library(janitor)
library(magrittr)
library(glue)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(gganimate)

palette_met <- met.brewer("Redon")[]

palette_list <-
  list(
    c(palette_met[1], palette_met[2]), 
    c(palette_met[3], palette_met[4]),
    c(palette_met[5], palette_met[6]),
    c(palette_met[7], palette_met[8]),
    c(palette_met[11], palette_met[12])
  )


gp <- gapminder %>%
  clean_names() %>%
  mutate(continent = as.character(continent))

country_colors <- gp %>%
  map2_df(
  .x = distinct(gp, continent) %>% pull(),
  .y = palette_list,
  .f = ~ {
    
    pal <- colorRampPalette(colors = c(.y[1], .y[2]))
    
    color_key <- gp %>%
      filter(continent == .x) %>%
      distinct(country) %>%
      mutate(color = pal(nrow(.)))
    
    return(color_key)
    
  }
)

country_colors <- deframe(country_colors)

p <- 
  ggplot(
    gp, 
    aes(x = gdp_percap, y = life_exp, size = pop, colour = country)
  ) +
  geom_label_repel(
    data = gp %>% filter(country == "Brazil"),
    aes(
      x = gdp_percap, 
      y = life_exp, 
      label = glue(
        "{country}\n",
        "{label_dollar(scale_cut = cut_short_scale(), accuracy = 1)(gdp_percap)}\n",
        "{round(life_exp)} Years"
      )
    ),
    inherit.aes = FALSE,
    point.size = NA,
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding = 1,
    nudge_y = -14,
    nudge_x = 0.5,
    show.legend = FALSE,
    alpha = 0.8,
    family = "Caladea",
    size = 3,
    label.size = 0.5,
    hjust = 0,
    xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)
  ) +
  geom_point(
    data = gp %>% filter(country == "Brazil"),
    color = "#000000",
    alpha = 1, 
    show.legend = FALSE
  ) +
  geom_point(
    data = gp %>% filter(country != "Brazil"),
    alpha = 0.8, 
    show.legend = FALSE
  ) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10(
    labels = label_dollar(scale_cut = cut_short_scale())
  ) +
  facet_wrap(~ continent) +
  theme_minimal_grid() +
  theme(
    plot.background = element_rect(fill = "#9d9da0"),
    plot.title = element_text(
      color = "#262429", 
      size = 18, 
      family = "Caladea",
      hjust = -1.2
    ),
    plot.subtitle = element_text(
      color = "#444149", 
      size = 12, 
      family = "Junicode", 
      face = "bold"
    ),
    axis.title = element_text(
      color = "#444149",
      size = 11, 
      family = "Junicode",
      face = "bold"
    ),
    axis.text = element_text(color = "#444149", size = 10, family = "Junicode"),
    strip.text = element_text(color = "#444149", size = 12, family = "Junicode"),
    plot.caption = element_text(
      color = "#262429", 
      size = 9, 
      family = "Caladea"
    ),
    panel.grid = element_line(color = "#444149", linetype = 2),
    axis.ticks = element_blank()
  ) +
  # Here comes the gganimate specific bits
  labs(
    title = "Global evolution of life expectancy and GPD per capta",
    subtitle = "Year: {frame_time}", 
    caption = "Hugo Tameirão Seixas | Data: Gapminder",
    x = 'GDP per capita (PPP dollars)', 
    y = 'Life Expectancy'
  ) +
  transition_time(year) +
  ease_aes('linear')
  
animate(
  p,
  width = 18,
  height = 12,
  units = "cm",
  res = 200,
  renderer = gifski_renderer(),
  device = "ragg_png"
)

anim_save("dataviz/figures/gapminder.gif")
