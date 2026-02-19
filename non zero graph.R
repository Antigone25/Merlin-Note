
library(tidyverse)
windowsFonts(
  Times = windowsFont("Times New Roman")
)
rf_lats <- read.csv("D:\\Desktop\\Nature Studies\\Merlin Note\\merlin rf lat no-zeros.csv")

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
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(size = 19),
    axis.title = element_text(face = "bold", size = 16),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

