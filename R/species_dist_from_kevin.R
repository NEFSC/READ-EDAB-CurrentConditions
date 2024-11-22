#summarize species distribution metrics for 2024 SOE report

#based on Kevin Friedland's output file,using spring trawl data through 2024
#datefile: "dhdc_7_2024_fallASS_sprDATA.csv"

#author: Sarah Weisberg
#contact: sarah.weisberg@noaa.gov

# Fri Nov 22 11:15:36 2024 ------------------------------

#load packages
library(here)
library(dplyr)

#read in data
#remove first column
#ASDIST = along-shelf distance
#DTEOC = distance to coast
dist <- read.csv(here::here("data-raw/kevin-output/dhdc_7_2024_fallASS_sprDATA.csv")) %>%
  dplyr::select(-X)

#calculate means for three metrics of interest
#ignoring 2020 because of survey disruptions
dist_mean <- dist %>%
  dplyr::group_by(YR) %>%
  dplyr::summarise(alongshelf = mean(ASDIST, na.rm = T), 
                   depth = mean(DEPTH, na.rm = T),
                   coast = mean(DTEOC, na.rm = T)) %>%
  dplyr::filter(!YR == 2020)

#write output
write.csv(x = dist_mean, file = here::here("data/species_dist_spring.csv"), row.names = FALSE)
