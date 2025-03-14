library(dplyr)
library(ggplot2)
library(tidyr)
library(rerddap)

erddap_info <- rerddap::info(datasetid = "nceiErsstv5", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

end_date <- data.frame(name = erddap_info$alldata$NC_GLOBAL$attribute_name,
                       value = erddap_info$alldata$NC_GLOBAL$value) %>%
  filter(name == "time_coverage_end") %>%
  mutate(value = base::as.Date(stringr::str_remove(value, "T12:00:00Z")),
         value = as.character(value)) %>%
  pull(value)

# This function downloads and prepares data based on user provided start and end dates
ERSST_sub_dl <- function(time_df, latitude = c(34, 46), longitude = c(-78, -62)){
  OISST_dat <- rerddap::griddap(datasetx = "nceiErsstv5_LonPM180",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                                time = c(time_df$start, time_df$end),
                                depth = c(0, 0),
                                latitude = latitude,
                                longitude = longitude,
                                fields = "sst")$data %>%
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>%
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) %>%
    dplyr::select(lon, lat, t, temp) %>%
    stats::na.omit()
}


dl_years = data.frame(start = seq(1854, lubridate::year(Sys.Date()) - 5, 5),
                      end = seq(1859, lubridate::year(Sys.Date()), 5)) %>%
  mutate(date_index = row_number(),
         start = paste0(start, "-01-01T12:00:00Z"),
         end = paste0(end, "-12-31T12:00:00Z"))

dl_years$end[dl_years$end == max(dl_years$end)] <- end_date

ERSST_data <- dl_years %>%
  dplyr::group_by(date_index) %>%
  dplyr::group_modify(~ERSST_sub_dl(.x)) %>%
  dplyr::ungroup() %>%
  dplyr::select(lon, lat, t, temp)

ERSST_month <- ERSST_data %>%
  mutate(lonlat = paste0(lon,lat),
         year = lubridate::year(t),
         month = lubridate::month(t)) %>%
  filter(lonlat %in% c("-7440", "-7438", "-7240", "-7044",
                       "-7042", "-7040", "-6844", "-6842")) %>%
  dplyr::select(year, month, temp) %>%
  group_by(year, month) %>%
  summarize(mean = mean(temp, na.rm = TRUE)) %>%
  pivot_wider(id_cols = year, names_from = month, values_from = mean)
# summarize(mean = mean(temp, na.rm = TRUE))

ERSST_year <- ERSST_data %>%
  mutate(lonlat = paste0(lon,lat),
         year = lubridate::year(t)) %>%
  filter(lonlat %in% c("-7440", "-7438", "-7240", "-7044",
                       "-7042", "-7040", "-6844", "-6842")) %>%
  dplyr::select(year, temp) %>%
  group_by(year) %>%
  summarize(mean = mean(temp, na.rm = TRUE))

write.csv(x = ERSST_year, file = here::here("data/annual_ersst.csv"), row.names = FALSE)
