
library(curl)
library(glue)
library(fs)
library(sf)
library(geobr)
library(terra)
library(magrittr)
library(sparklyr)
library(dplyr)
library(tibble)
library(readr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggfx)
library(patchwork)
library(scico)
library(stringr)

dir_create("dataviz/temp")

curl_download(
  "https://zenodo.org/record/7484163/files/figures.tar.gz",
  "dataviz/temp/figures.tar.gz"
)

untar(
  tarfile = "dataviz/temp/figures.tar.gz",
  exdir = "dataviz/temp/"
)

amazon <- read_biomes() %>%
  filter(code_biome == 1) %>%
  st_transform("EPSG:4326")

states <- read_state() %>%
  st_transform("EPSG:4326") %>%
  st_intersection(amazon)

c_hex <- 
  read_csv("dataviz/temp/figures/fig_4_hex.csv") %>%
  rename(geometry = "WKT") %>%
  st_as_sf(wkt = "geometry", crs = "EPSG:4326")

frequency <- read_csv("dataviz/temp/figures/fig_4_freq.csv")

hr_raster <- read_csv("dataviz/temp/figures/fig_4_hr.csv")

hr_bbox <-
  st_as_sfc(
    st_bbox(
      c(
        xmin = -52.3262,
        xmax = -52.0786,
        ymax =  -12.3993,
        ymin =  -12.6592
      ),
      crs = st_crs(4326)
    )
  )

dist_map <- ggplot() +
  with_shadow(
    geom_sf(
      data = amazon,
      fill = "#747171",
      color = "transparent"
    ),
    sigma = 3,
    x_offset = 3,
    y_offset = 3
  ) +
  geom_sf(
    data = states,
    fill = "transparent",
    color = "#484646"
  ) +
  geom_sf(
    data = c_hex,
    aes(fill = c_length),
    linewidth = 0.05
  ) +
  geom_sf(
    data = hr_bbox,
    fill = "transparent",
    color = "#c50505"
  ) +
  ggtitle(
    "Distribution of conversion from forest to agriculture in the Amazon"
  ) +
  guides(
    fill = guide_colourbar(
      barwidth = 0.5,
      barheight = 8,
      ticks = FALSE,
      frame.colour = "black"
    )
  ) +
  scale_fill_scico(
    palette = "batlow",
    breaks = c(0, 17, 35),
    name = "Conversion Length (year)"
  ) +
  theme_void(base_family = "roboto") +
  theme(
    legend.position = c(0.14, 0.77),
    text = element_text(size = 11)
  )

hr_map <- ggplot() +
  geom_raster(
    data = hr_raster,
    aes(x = x, y = y, fill = c_length)
  ) +
  scale_fill_scico(palette = "batlow") +
  scale_x_continuous(expand = c(0.0004, 0.0004)) +
  scale_y_continuous(expand = c(0.0004, 0.0004)) +
  coord_fixed() +
  theme_void(base_family = "roboto") +
  theme(
    legend.position = "",
    panel.background = element_rect(fill = "#747171"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

hist_plot <- ggplot() +
  geom_bar(
    data = frequency,
    aes(x = c_length, fill = factor(c_length)),
    color = "black",
    lwd = 0.2
  ) +
  scale_fill_scico_d(palette = "batlow") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void(base_family = "roboto") +
  theme(
    legend.position = ""
  )

full_map <- dist_map +
  inset_element(hr_map, 0.76, 0, 1, 0.34, align_to = "panel") +
  inset_element(hist_plot, 0, 0, 0.5, 0.4, align_to = "panel")

ggsave(
  "dataviz/figures/map.svg",
  full_map,
  width = 17,
  height = 12.968,
  units = "cm",
  dpi = 300
)

dir_delete("dataviz/temp/")
