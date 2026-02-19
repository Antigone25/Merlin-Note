library(tidyverse)
library(lubridate)
merlin_aacomp <- merlin_afroasia %>% 
  filter(ALL.SPECIES.REPORTED == 1)

merlin_aacomp1 <- merlin_aacomp %>% 
  filter(
    LATITUDE > 50
  )
merlin_aacomp2 <- merlin_aacomp %>% 
  filter(
    LATITUDE < 48
  )
nrow(merlin_aacomp1) #436 complete records in Asian breeding range
nrow(merlin_aacomp2) #6297 complete records in Afro-Asian non-breeding range
head(merlin_aacomp1)
write.csv(merlin_aacomp1, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin_breeding.csv")
write.csv(merlin_aacomp2, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin_nonbreeding.csv")
unique(merlin_aacomp$COUNTRY)
strong_breeding_codes <- c(
  "M", "C ", "CN", "NB", "UN", "ON",
  "FL", "FY", "FS", "NE", "NY"
)
colnames(merlin_aacomp)
merlin_strong_breeding <- merlin_afroasia %>%
  filter(
BREEDING.CODE %in% strong_breeding_codes,
!is.na(BREEDING.CODE)
  )

nrow(merlin_strong_breeding) #16 strong evidence breeding recordsfrom all types of lists
table(merlin_strong_breeding$BREEDING.CODE, useNA = "ifany")
#CN FL FY NE ON 
# 1  2  2  1 10
breeding_merlin_photo <- merlin_strong_breeding %>%
  filter(
    !is.na(BREEDING.CODE) & BREEDING.CODE != "",
    HAS.MEDIA == 1
  )

min_lat_breeding <- breeding_merlin_photo %>%
  summarise(min_lat = min(LATITUDE, na.rm = TRUE))
min_lat_breeding #50.49564

southernmost_breeding_record <- breeding_merlin_photo %>%
  filter(LATITUDE == min(LATITUDE, na.rm = TRUE)) %>%
  select(
    COUNTRY,
    STATE,
    LOCALITY,
    LATITUDE,
    LONGITUDE,
    OBSERVATION.DATE,
    BREEDING.CODE,
    HAS.MEDIA,
    APPROVED
  )

southernmost_breeding_record #50.49564

merlin_nonbreeding <- merlin_aacomp %>%
  filter(
    LATITUDE < 48,
    ALL.SPECIES.REPORTED == 1
  )
nrow(merlin_nonbreeding) #6297 records in non-breeding range of CA/WA/EA flyways
monthly_counts <- merlin_nonbreeding %>%
  mutate(month = month(OBSERVATION.DATE)) %>%
  group_by(month) %>%
  summarise(n = n(), .groups = "drop")

non_breed_recs <- ggplot(monthly_counts, aes(x = month, y = n)) +
  geom_col() +
  geom_vline(xintercept = 12, linetype = "dashed") +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x = "Month",
    y = "Number of records",
    title = "Seasonal distribution of Merlin records in the Europe-Asia-Africa Flyway non-breeding range"
  )
non_breed_recs
#plot 3:

keep_codes <- c(
  "M", "C", "CN", "NB", "UN", "ON",
  "FL", "CF", "FY", "FS", "NE", "NY"
)

merlin_breeding_strict <- merlin_aacomp %>%
  filter(
    !is.na(BREEDING.CODE) &
      str_detect(
        BREEDING.CODE,
        paste0("\\b(", paste(keep_codes, collapse = "|"), ")\\b")
      )
  )
nrow(merlin_breeding_strict)
table(merlin_breeding_strict$BREEDING.CODE, useNA = "ifany")
breeding_merlin_photo <- merlin_strong_breeding %>%
  filter(
    !is.na(BREEDING.CODE) & BREEDING.CODE != "",
    HAS.MEDIA == 1,
    APPROVED == 1
  )

nrow(breeding_merlin_photo) #4 breeding photographic approved records

min_lat_breeding <- breeding_merlin_photo %>%
  summarise(min_lat = min(LATITUDE, na.rm = TRUE)) #50.49564
min_lat_breeding

cutoff_lat <- floor(min_lat_breeding$min_lat) - 2 #2 degree buffer
cutoff_lat #48

merlin_nonbreeding <- merlin_aacomp %>%
  filter(
    LATITUDE < cutoff_lat,
    APPROVED == 1,
    HAS.MEDIA == 1
      )
nrow(merlin_nonbreeding) #732 approved non-breeding photographic records below 48 degrees lat

merlin_winter <- merlin_aacomp %>%
  mutate(month = month(OBSERVATION.DATE)) %>%
  filter(month %in% c(10, 11, 12, 1, 2, 3))
nrow(merlin_winter) #5568 records in winter

median_lat <- median(merlin_winter$LATITUDE, na.rm = TRUE)
median_lat #37.39086

quantile(merlin_winter$LATITUDE, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(merlin_winter$LATITUDE, probs = c(0.25, 0.3, 0.5), na.rm = TRUE)
southofbpl <- mean(merlin_winter$LATITUDE <= 23.22, na.rm = TRUE) * 100
merlin_ind <- merlin_winter %>% 
  filter(COUNTRY == "India")
southeastofbpl <- mean(merlin_ind$LATITUDE <= 23.22 & merlin_ind$LONGITUDE >= 77.32,
                       na.rm = TRUE) * 100
southofbpl #1.275144% records lie south of Bhopal latitudinally
southeastofbpl
medlat <- ggplot(merlin_winter, aes(x = LATITUDE)) +
  geom_density(fill = "brown", alpha = 0.6) +
  geom_vline(xintercept = 23.13, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(merlin_winter$LATITUDE, na.rm = TRUE),
             linetype = "dotted", linewidth = 1) +
  labs(
    x = "Latitude (°N)",
    y = "Density",
    title = "Latitudinal distribution of winter Merlin records",
    subtitle = "Dashed line = Bhopal (23.13°N); Dotted line = median winter latitude"
  ) +
  theme_classic()
medlat

spatdist2 <- ggplot(merlin_winter, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(alpha = 0.3) +
  annotate("point", x = 77.19, y = 23.13, colour = "red", size = 3) +
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°N)",
    title = "Spatial distribution of winter Merlin records in Asia, Africa and Europe",
    subtitle = "Red point = Bhopal record"
  ) +
  theme_classic()
spatdist2
