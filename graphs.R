library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(tidyverse)

windowsFonts(
  Times = windowsFont("Times New Roman")
)
theme_note <- theme_classic(base_family = "Times", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )
col_bg   <- "#5A6E73"   # muted blue-grey for background
col_ac   <- "#a52a2a"   # deep red for Bhopal / focal line
col_fill <- "#E7D99F"   
season_dist1 <- ggplot(monthly_counts, aes(x = month, y = n)) +
  geom_col(fill = col_fill, width = 0.8) +
  geom_vline(xintercept = 12, linetype = "dotted",
             linewidth = 0.6, colour = col_ac) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  labs(
    x = NULL,
    y = "Number of observations",
    title = "Seasonal distribution of Merlin (<40°N)",
    subtitle = "Red Line Denotes Bhopal",
    caption = "Data: eBird. 2025") +
  theme_note +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 22))

season_dist1

medlat <- ggplot(merlin_winter, aes(x = LATITUDE)) +
  geom_density(fill = col_fill, alpha = 0.7, colour = NA) +
  geom_vline(xintercept = 23.22, linetype = "dashed",
             linewidth = 0.8, colour = col_ac) +
  geom_vline(
    xintercept = median(merlin_winter$LATITUDE, na.rm = TRUE),
    linetype = "dotted",
    linewidth = 0.7,
    colour = "grey30"
  ) +
  labs(
    x = "Latitude (°N)",
    y = "Density",
    title = "Latitudinal distribution of Merlin records in winter",
    subtitle = "Dashed line = Bhopal; dotted line = median winter latitude"
  ) +
  theme_note

medlat

spatdist2 <- ggplot(merlin_winter, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(size = 1, alpha = 0.35, colour = col_bg) +
  annotate("point", x = 77.32, y = 23.22,
           colour = col_ac, size = 1.5, shape = 3) +
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°N)",
    title = "Spatial distribution of winter Merlin records",
    subtitle = "Cross indicates Bhopal record"
  ) +
  theme_note

spatdist2

library(tidyverse)
windowsFonts(
  Times = windowsFont("Times New Roman")
)
rf_lats <- read.csv("D:\\Desktop\\Nature Studies\\Merlin Note\\merlin rf lat.csv")

lats_bin <- rf_lats %>%
  mutate(lat_bin = cut(Latitude, breaks = seq(-40, 60, by = 5))) %>%
  group_by(lat_bin) %>%
  summarise(
    lat_mid = mean(Latitude, na.rm = TRUE),  # midpoint of the bin
    RF = mean(RF, na.rm = TRUE)             # mean reporting frequency per bin
  )

polyfil <- "#EBD58D"
polyline <- "#D0AC77"
ggplot() +
  geom_col(data = lats_bin, aes(x = lat_mid, y = RF),
           size = 1, alpha = 0.3, fill = polyfil) +
  # Binned points and line
  geom_line(data = lats_bin, aes(x = lat_mid, y = RF), 
            color = polyfil, size = 1) +
  geom_point(data = lats_bin, aes(x = lat_mid, y = RF), 
             color = polyline, size = 3) +
  # Smoothed trend from raw data
  geom_smooth(data = rf_lats, aes(x = Latitude, y = RF),
              method = "loess", span = 0.265, color = "#b87333", 
              size = 2.5, alpha = 0.2, fill = "#efdecd") +
  # Vertical line at latitude of interest
  geom_vline(xintercept = 23.22, linetype = "dashed", color = "#a52a2a", size = 1, alpha = 0.4) +
  
  # Labels and theme
  labs(
    x = "Latitude (°N)", 
    y = "Reporting Frequency (RF)", 
    title = "Merlin Reporting Frequency in Asia and Africa",
    subtitle = "Loess with 5° latitude bins,
Red Dashed Line indicates Bhopal (23.22)",
    caption = "Data: eBird. 2025"
  ) +
  theme_minimal(base_family = "Times", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 26),
    plot.subtitle = element_text(size = 21),
    axis.title = element_text(face = "bold", size = 20),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

