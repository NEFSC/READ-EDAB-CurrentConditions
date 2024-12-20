#calculating spring and fall transition dates for 2024 SOE report

#based on Kevin Friedland's output file, OISST daily values assigned to EPUs
#datefile: "TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv"

#author: Sarah Weisberg
#contact: sarah.weisberg@noaa.gov

# Fri Dec 20 16:44:49 2024 ------------------------------

#install packages
library(here)
library(dplyr)
library(zoo)
library(ggplot2)

#load full dataset
daily_sst_full <- read.csv(here::here("data-raw/kevin-output/TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv"))

#trim
#keep only EPUs of interest = MAB, GOM, GBK
#remove 1981 (partial data)
daily_sst <- daily_sst_full %>% 
  dplyr::select(Location, Y, M, D, Mean,Date) %>%
  dplyr::filter(Location %in% c("MAB","GOM","GBK"),
                Y >=1982)
  
#calculate regional averages
#30 year baseline = 1991-2020
mean_sst <- daily_sst %>%
  dplyr::group_by(Location) %>%
  dplyr::filter(Y >=1991 & Y <= 2020) %>%
  dplyr::summarise(mean_sst = mean(Mean))

#add mean to daily
daily_sst <- dplyr::left_join(daily_sst,mean_sst,by="Location") %>%
  dplyr::group_by(Location) %>%
  dplyr::mutate(roll_mean_sst = zoo::rollmean(Mean,k=5,align = "center",fill=NA))
#gives you NAs for first 2, last 2 entries, but otherwise the same output as using movingFun function

#calculate transition days
#spring transition = 1st day where 5 day mean is above long-term average
spr_trans <- daily_sst %>%
  dplyr::group_by(Location) %>%
  dplyr::filter(roll_mean_sst > mean_sst) %>%
  dplyr::group_by(Location,Y) %>%
  dplyr::summarise(sprtrans = min(Date))

#fall transition = 1st day in last quarter of year where 5 day mean is below long-term average
fall_trans <- daily_sst %>%
  dplyr::group_by(Location) %>%
  dplyr::filter(roll_mean_sst < mean_sst & Date >= 275) %>%
  dplyr::group_by(Location,Y) %>%
  dplyr::summarise(falltrans = min(Date))

#max date = 
max_date <- daily_sst %>%
  dplyr::group_by(Location,Y) %>%
  dplyr::filter(roll_mean_sst == max(roll_mean_sst)) %>%
  dplyr::select(Location, Y, Date)

#merge
#calculate summer length = fall transition - spring transition
trans_dates <- dplyr::left_join(spr_trans,fall_trans,by=c("Location","Y")) %>%
  dplyr::mutate(sumlen = falltrans-sprtrans) 

trans_dates <- dplyr::left_join(trans_dates,max_date,by=c("Location","Y")) %>%
  dplyr::rename(EPU = Location, Year = Y, maxday = Date)

#write output file
write.csv(x = trans_dates, file = here::here("data/trans_dates.csv"), row.names = FALSE)



  