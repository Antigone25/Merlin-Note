rm(list = ls())

library(tidyverse)
library(sf)
library(ggplot2)
library(maps)
data <- read.delim("D:\\Desktop\\Nature Studies\\Merlin Note\\merlin raw.txt", stringsAsFactors = FALSE)
colnames(data)
data <- data %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))
data <- data %>%
  mutate(year = lubridate::year(`OBSERVATION.DATE`)) %>%
  filter(year >= 2010)
head(data$OBSERVATION.DATE)
world <- map_data("world")
map1 <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey70",
    linewidth = 0.2
  ) +
  stat_density_2d(
    data = data,
    aes(x = LONGITUDE, y = LATITUDE, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.6,
    bins = 30
  ) +
  scale_fill_viridis_c(name = "Observation density") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    title = "Global observation density of <Species name>",
    x = "Longitude",
    y = "Latitude"
  )
map1

data_sf <- st_as_sf(
  data,
  coords = c("LONGITUDE", "LATITUDE"),
  crs = 4326
)

grid <- st_make_grid(
  data_sf,
  cellsize = 1,   # 1° × 1° grid; change to 0.5 for finer
  square = TRUE
)

grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)

counts <- st_join(grid_sf, data_sf) %>%
  group_by(grid_id) %>%
  summarise(n = n(), geometry = first(geometry))

map2 <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70") +
  geom_sf(data = counts, aes(fill = n), color = NA) +
  scale_fill_viridis_c(trans = "log10", name = "No. of records") +
  theme_minimal() +
  labs(title = "Global distribution of <Species name>")
map2
