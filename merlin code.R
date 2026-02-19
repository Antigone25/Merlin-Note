library(tidyverse)
library(lubridate)
merlin_afroasia <- read.csv("D:\\Desktop\\Nature Studies\\Merlin Note\\merlin country filter afroasia.csv")
colnames(merlin_afroasia)
nrow(merlin_afroasia) #8546
merlin_aacomp <- merlin_afroasia %>% 
  filter(ALL.SPECIES.REPORTED == 1)
nrow(merlin_aacomp) #6835 complete lists
#proportion metrics
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

countryfilter <- merlin_aacomp %>%
  filter(COUNTRY.CODE %in% subcontinent_countries)
nrow(countryfilter) #453 records in Indian subcontinent
statefilter <- merlin_aacomp %>%
  filter(STATE %in% states_of_interest)
view(statefilter) # 1 record, from UP, from central India
ind <- "India"
india <- merlin_aacomp %>% 
  filter(COUNTRY %in% ind)
nrow(india)
write.csv(india, "D:\\Desktop\\Nature Studies\\Merlin Note\\merlin india.csv")
gpr <-   india %>% 
  summarise(
    total_records = n(),
    centralindia = sum(STATE %in% states_of_interest, na.rm = TRUE),
    percentage = (centralindia / total_records) * 100
  )
gpr 
#total_records centralindia percentage
#         431            1  0.2320186

nrow(merlin_aacomp) #6835 records from asia and africa
####
####
####

merlin_aacomp$OBSERVATION.DATE<- as.Date(
  merlin_aacomp$OBSERVATION.DATE,
  format = "%Y-%m-%d"
)
head(merlin_aacomp$OBSERVATION.DATE)
summary(merlin_aacomp$OBSERVATION.DATE)

merlin_aacomp$month <- month(merlin_aacomp$OBSERVATION.DATE)
merlin_aacomp$year <- year(merlin_aacomp$OBSERVATION.DATE)
winter_merlin <- merlin_aacomp %>%
  filter(year >= 2010) %>% 
  filter(month %in% c(10, 11, 12, 1, 2, 3))
merlin_win_nonbreed <- winter_merlin %>% 
  filter(LATITUDE <= 40)
nrow(merlin_win_nonbreed)
nrow(winter_merlin) #5060 records from october to march, 2010-2025
range(winter_merlin$LATITUDE, na.rm = TRUE) #12.99594 <-> 68.91003
nrow(winter_merlin)
nrow(winter_merlin)/nrow(merlin_aacomp)*100 #74.03072% records in Afro-asia occur in winter
# Plot
latdist <- ggplot(winter_merlin, aes(x = LATITUDE)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 23.22, linetype = "dashed") +
  labs(
    x = "Latitude (°N)",
    y = "Number of records",
    title = "Latitudinal distribution of winter Merlin records in Asia and Africa"
  )
latdist

#merlin_aacomp <- merlin_aacomp %>%
#  mutate(
#    OBSERVATION.DATE = ymd(OBSERVATION.DATE)
#  )

monthly_counts <- merlin_aacomp %>%
  mutate(month = month(OBSERVATION.DATE)) %>%
  group_by(month) %>%
  summarise(n = n(), .groups = "drop")

seasondist <- ggplot(monthly_counts, aes(x = month, y = n)) +
  geom_col() +
  geom_vline(xintercept = 12, linetype = "dashed") +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x = "Month",
    y = "Number of records",
    title = "Seasonal distribution of Merlin records in Africa and Asia"
  )
seasondist
