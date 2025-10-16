rm(list=ls()) # Clear de "Global Environment"
setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/computational-history")

library(tidyverse)
library(sf)
library(tmap)
dist_sh <- read_sf("data/dist-1860/dist_1860.shp")
map <- dist_sh |>
  tm_shape() +
  tm_polygons(fill = "literacy_m", lwd = 0.5,
              fill.scale = tm_scale_intervals(
                style = "fixed",
                breaks = c(0, 10, 20, 30, 40, 50, 60, Inf),
                values = "brewer.reds"), # color palette
              fill.legend = tm_legend(
                title = "",
                position = c("right", "bottom")))

tmap_save(map, "images/map_lit_1860.jpg", dpi = 600)