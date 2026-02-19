library(tidyverse)
library(lubridate)

setwd("D:/Desktop/Nature Studies/Merlin Note")

merlin <- read.delim("merlin raw.txt", stringsAsFactors = FALSE)
write.csv(merlin, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin check.csv")
head(unique(sort(merlin$OBSERVATION.DATE))) #1st record 1-1-2010
subset(merlin$COUNTRY == "Morocco")
nrow(merlin) #753614
subcontinent_countries <- c(
  "IN",  # India
  "PK",  # Pakistan
  "NP",  # Nepal
  "BD",  # Bangladesh
  "LK",  # Sri Lanka
  "BT",  # Bhutan
  "AF",  # Afghanistan
  "MV"   # Maldives
)
states_of_interest <- c("Madhya Pradesh", "Uttar Pradesh", "Jharkhand", "Maharashtra",
                        "Karnataka", "Tamil Nadu", "Kerala,", "Telangana", "Andhra Pradsh",
                        "Orissa")

countryfilter <- merlin %>%
  filter(COUNTRY.CODE %in% subcontinent_countries)
nrow(countryfilter) #745 records in Indian subcontinent, all in India since 2010
write.csv(countryfilter, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin asia.csv")
gpr <-   countryfilter %>% 
  summarise(
  total_records = n(),
  centralindia = sum(STATE %in% states_of_interest, na.rm = TRUE),
  percentage = (centralindia / total_records) * 100
)
gpr 
#  total_records centralindia percentage
#1           745            1  0.1342282

north_america <- c(
  "Canada",
  "United States",
  "Mexico",
  "Greenland",
  "Bermuda",
  "Saint Pierre and Miquelon"
)
south_america <- c(
  "Argentina",
  "Bolivia",
  "Brazil",
  "Chile",
  "Colombia",
  "Ecuador",
  "Guyana",
  "Paraguay",
  "Peru",
  "Suriname",
  "Uruguay",
  "Venezuela",
  "French Guiana",
  "Falkland Islands"
)
caribbean <- c(
  "Anguilla",
  "Antigua and Barbuda",
  "Aruba",
  "Bahamas",
  "Barbados",
  "Bonaire, Sint Eustatius, and Saba",
  "Cayman Islands",
  "Cuba",
  "Curaçao",
  "Dominica",
  "Dominican Republic",
  "Grenada",
  "Guadeloupe",
  "Haiti",
  "Jamaica",
  "Martinique",
  "Montserrat",
  "Puerto Rico",
  "Saint Barthélemy",
  "Saint Kitts and Nevis",
  "Saint Lucia",
  "Saint Martin",
  "Saint Vincent and the Grenadines",
  "Sint Maarten",
  "Trinidad and Tobago",
  "Turks and Caicos Islands",
  "Virgin Islands, British",
  "Virgin Islands, U.S."
)
central_america <- c(
  "Belize",
  "Costa Rica",
  "El Salvador",
  "Guatemala",
  "Honduras",
  "Nicaragua",
  "Panama"
)
amer
merlin <- merlin %>% 
  filter(!COUNTRY %in% americas) %>% 
  slice(-(1:3))
head(merlin)
write.csv(merlin, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin ex-americas.csv")
nrow(merlin)

merlin_east <- read.csv("D:\\Desktop\\Nature Studies\\Merlin Note\\merlin non breed.csv")

nrow(merlin_east) #35886 records from europe, asia and africa, all seasons
head(merlin_east)
merlin_east$OBSERVAT_1 <- as.Date(
  merlin_east$OBSERVAT_1,
  format = "%Y/%m/%d"
)

summary(merlin_east$OBSERVAT_1)

merlin_east$month <- month(merlin_east$OBSERVAT_1)

winter_merlin <- merlin_east %>%
  filter(month %in% c(10, 11, 12, 1, 2, 3))

range(winter_merlin$LATITUDE, na.rm = TRUE) #17.04070 67.53245
nrow(winter_merlin) #23873 winter month records

# Plot
latdist <- ggplot(winter_merlin, aes(x = LATITUDE)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 23.22, linetype = "dashed") +
  labs(
    x = "Latitude (°N)",
    y = "Number of records",
    title = "Latitudinal distribution of winter Merlin records in Europe, Asia, and Africa"
  )
latdist

#merlin_east <- merlin_east %>%
#  mutate(
#    OBSERVATION.DATE = ymd(OBSERVATION.DATE)
#  )

monthly_counts <- merlin_east %>%
  mutate(month = month(OBSERVAT_1)) %>%
  group_by(month) %>%
  summarise(n = n(), .groups = "drop")

seasondist <- ggplot(monthly_counts, aes(x = month, y = n)) +
  geom_col() +
  geom_vline(xintercept = 12, linetype = "dashed") +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x = "Month",
    y = "Number of records",
    title = "Seasonal distribution of Merlin records in Europe, Africa and Asia"
  )
seasondist
