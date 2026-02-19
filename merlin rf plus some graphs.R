# Load required packages
library(dplyr)
library(data.table)
library(tidyverse)

# Read eBird data (example file with columns: SAMPLING_EVENT_ID, LATITUDE, LONGITUDE, etc.)

ebird <- fread("D:\\Desktop\\Nature Studies\\Merlin Note\\sampeffdata\\sampeffraw.txt", sep = "\t", showProgress = TRUE)
colnames(ebird)
# Remove duplicate checklists
ebird_unique <- ebird %>%
  mutate(
    ID = ifelse(
      !is.na(`GROUP IDENTIFIER`) & `GROUP IDENTIFIER` != "",
      `GROUP IDENTIFIER`,
      `SAMPLING EVENT IDENTIFIER`
    )
  ) %>%
  distinct(ID, .keep_all = TRUE)

write.csv(ebird_unique, "D:\\Desktop\\Nature Studies\\Merlin Note\\ Merlin Dupsfree fin.csv")

nrow(ebird) #1844743
unique(ebird$COUNTRY)
nrow(ebird_unique) #1451041: ~450000 dups removed
countries <- c(
  # Africa
  "Niger", "Nigeria", "Chad", "Cameroon", "Central African Republic",
  "Sudan", "South Sudan", "Eritrea", "Egypt", "Ethiopia", "Somalia",
  "Kenya", "Uganda", "Rwanda", "Burundi", "Democratic Republic of Congo", "Congo", "Gabon",
  "Equatorial Guinea", "Tanzania", "Zambia", "Angola", "Malawi", "Mozambique",
  "Zimbabwe", "Botswana", "Namibia", "Eswatini", "Lesotho", "South Africa", "Madagascar",
  
  # Asia
  "Oman", "Yemen", "Saudi Arabia", "United Arab Emirates", "Bangladesh", "Myanmar",
  "Laos", "Vietnam", "Thailand", "Cambodia", "Malaysia", "Indonesia",
  "Philippines", "Brunei", "Russia", "Kazakhstan", "Mongolia", "China", "Uzbekistan",
  "Kyrgyzstan", "Turkmenistan", "Tajikistan", "North Korea", "South Korea",
  "Japan", "Azerbaijan", "Georgia", "Türkiye", "Syria", "Lebanon", "Israel",
  "Palestinian Territory", "Jordan", "Iran", "Iraq", "Kuwait", "Afghanistan", "Pakistan",
  "Bhutan", "Nepal", "Taiwan",
  # Indian regions (grouped as India)
  "India"
)
ebird_country <- ebird_unique %>% 
  filter(COUNTRY %in% countries)
unique(ebird_country$COUNTRY)
nrow(ebird_country)
merlin_nonbreed <- ebird_country %>% 
  filter(LATITUDE <= 40)
nrow(merlin_nonbreed)
indi <- "India"
ebird_india <- ebird_unique %>% 
  filter(COUNTRY %in% indi)
nrow(ebird_india)
head(ebird_india$`OBSERVATION DATE`)
ebird_india$`OBSERVATION DATE` <- as.Date(
  ebird_india$`OBSERVATION DATE`,
  format = "%Y-%m-%d"
)
ebird_india$year <- year(ebird_india$`OBSERVATION DATE`)
ebird_india <- ebird_india %>% 
  filter(year >= 2010)
nrow(ebird_india) #487

ebird_latbins <- ebird_country %>%
  mutate(
    lat_bin = floor(LATITUDE)   # eg 19.7 → 19
  )
effort_lat <- ebird_latbins %>%
  group_by(lat_bin) %>%
  summarise(
    total_effort_hours = sum((`DURATION MINUTES`/60), na.rm = TRUE),
    n_lists = n(),
    .groups = "drop"
  )
total_lists <- ebird_latbins %>%
  distinct(ID, lat_bin) %>%
  count(lat_bin, name = "total_lists")
view(total_lists)
view(effort_lat)

country_centroids <- ebird_country %>%
  group_by(COUNTRY) %>%
  summarise(
    centroid_lat = weighted.mean(LATITUDE, w = `DURATION MINUTES`, na.rm = TRUE),
    total_effort = sum(`DURATION MINUTES`, na.rm = TRUE),
    n_lists = n(),
    .groups = "drop"
  )

view(country_centroids)
centroid <- ebird_country %>%
  filter(COUNTRY == "Egypt") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Cambodia") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "India") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "United Arab Emirates") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Vietnam") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Oman") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Thailand") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Philippines") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Myanmar") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )
print(centroid)
nrow(ebird_country)
write.csv(ebird_country, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin country filter afroasia.csv")
centroid <- ebird_country %>%
  filter(COUNTRY == "Saudi Arabia") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )
print(centroid)
#North Asia
centroid <- ebird_country %>%
  filter(COUNTRY == "Russia") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Kazakhstan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Mongolia") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "China") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Uzbekistan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Kyrgyzstan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Turkmenistan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Tajikistan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "South Korea") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Japan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Japan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Georgia") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)

centroid <- ebird_country %>%
  filter(COUNTRY == "Türkiye") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Lebanon") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Israel") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Palestinian Territory") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Jordan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Iran") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Iraq") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Kuwait") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Afghanistan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Pakistan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Nepal") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)
centroid <- ebird_country %>%
  filter(COUNTRY == "Taiwan") %>% 
  summarise(
    centroid_lat = mean(LATITUDE, na.rm = TRUE),
    centroid_lon = mean(LONGITUDE, na.rm = TRUE)
  )

# Print result
print(centroid)

rf_lats <- read.csv("D:\\Desktop\\Nature Studies\\Merlin Note\\merlin rf lat.csv")

rflat <- ggplot(rf_lats, aes(x = Latitude, y = RF)) +
  geom_point(size = 2) +
  labs(
    x = "Latitude (°)",
    y = "Reporting frequency (%)",
    title = "Latitudinal pattern in Merlin reporting frequency"
  ) +
  theme_classic()
rflat
rfsmooth <- ggplot(rf_lats, aes(x = Latitude, y = RF)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic() +
  labs(
    x = "Latitude (°)",
    y = "Reporting frequency (%)"
  )
rfsmooth

ggplot(rfcurve, aes(x = Latitude, y = RF)) +
  geom_line() +
  geom_point() +
  theme_classic()

polyfil <- "#EBD58D"
polyline <- "#D0AC77"

ggplot(rf_lats, aes(Latitude, RF)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE
  )
lats_bin <- rf_lats %>%
  mutate(lat_bin = cut(Latitude, breaks = seq(-40, 60, by = 5))) %>%
  group_by(lat_bin) %>%
  summarise(
    lat_mid = mean(Latitude, na.rm = TRUE),  # midpoint of the bin
    RF = mean(RF, na.rm = TRUE)             # mean reporting frequency per bin
  )

ggplot(lats_bin, aes(x = lat_mid, y = RF)) +
  geom_point() +
  geom_line() +
  geom_smooth(data = rf_lats, aes(x = Latitude, y = RF)) +
  geom_vline(xintercept = 23.22)
library(ggplot2)
library(dplyr)

# Summarize RF by latitude bins
lats_bin <- rf_lats %>%
  mutate(lat_bin = cut(Latitude, breaks = seq(-40, 60, by = 5))) %>%
  group_by(lat_bin) %>%
  summarise(
    lat_mid = mean(Latitude, na.rm = TRUE),
    RF = mean(RF, na.rm = TRUE)
  )

# Plot
ggplot() +
  # Smoothed trend from raw data
  geom_smooth(data = rf_lats, aes(x = Latitude, y = RF),
              method = "loess", span = 0.2, color = "#b87333", fill = "#efdecd", alpha = 0.3) +
  
  # Binned points and line
  
  geom_line(data = lats_bin, aes(x = lat_mid, y = RF), 
            color = polyfil, size = 1) +
  geom_point(data = lats_bin, aes(x = lat_mid, y = RF), 
             color = polyline, size = 3) +
  
  # Vertical line at latitude of interest
  geom_vline(xintercept = 23.22, linetype = "dashed", color = "#a52a2a", size = 1) +
  
  # Labels and theme
  labs(
    x = "Latitude (°N)", 
    y = "Reporting Frequency (RF)", 
    title = "Merlin Reporting Frequency in Asia and Africa",
    subtitle = "Loess with 5° latitude bins,
Red Line indicates Bhopal",
    caption = "Data: eBird. 2025"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
