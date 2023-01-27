
library(sf)
library(geobr)
library(osmdata)
library(cowplot)
library(ggfx)
library(extrafont)
library(glue)
library(tidyverse)

states <- read_state() %>%
  mutate(centroid = st_centroid(geom))

states_plot <- states %>%
  group_by(name_state) %>%
  group_map(
    .keep = TRUE,
    ~ {
      
      x_centroid <- .x$centroid[[1]][1]
      y_centroid <- .x$centroid[[1]][2]
      
      print(.x$name_state[1])
      
      # roads <- st_bbox(.x) %>%
      #   opq(timeout = 500) %>%
      #   add_osm_feature(
      #     key = "highway",
      #     value = c("motorway", "trunk")
      #   ) %>%
      #   osmdata_sf()

      # roads <- roads$osm_lines %>%
      #   select(geometry) %>%
      #   st_transform(crs = st_crs(.x)) %>%
      #   st_intersection(.x$geom) %>%
      #   st_simplify()
      
      p <- .x %>%
        ggplot() +
        with_shadow(
          geom_sf(fill = "#6f6161"),
          x_offset = 1, y_offset = 1
        ) +
        # geom_sf(
        #   data = roads,
        #   color = "#000000",
        #   linewidth = 0.1,
        #   alpha = 0.4
        # ) +
        coord_sf(
          xlim = c(x_centroid + 9, x_centroid - 9),
          ylim = c(y_centroid + 8, y_centroid - 8)
        ) +
        labs(caption = .x$name_state[1]) +
        theme_void(base_family = "Yrsa") +
        theme(
          plot.caption = 
            element_text(
              hjust = 0.5,
              size = rel(0.8),
              family = "Yrsa",
              face = "bold",
              color = "#372222",
              margin = margin(t = -10)
            ),
          plot.margin = unit(c(0.3, 0, 0.3, 0), "lines")
        )
    
      gc()
        
      return(p)
      
    }
  ) %>%
  plot_grid(
    plotlist = ., 
    labels = "Unidades Federativas do Brasil",
    label_size = 24,
    label_colour = "#372222",
    label_fontfamily = "Yrsa",
    hjust = 0,
    label_x = 0.02,
    ncol = 3, 
    align = "v"
  ) +
  theme(
    plot.background = element_rect(fill = "#864b4b")
  )

p <- add_sub(
  plot = states_plot,
  label = glue("Hugo Tameirão Seixas | Data: IBGE"),
  x = 0.98,
  hjust = 1,
  size = 9,
  fontfamily = "Yrsa",
  color = "#372222"
)

ggsave(
  "dataviz/figures/brazil_states.svg", 
  p, 
  dpi = 300,
  width = 15,
  height = 35,
  units = "cm"
)
