library(lubridate)
library(tidyverse)

# Melbourne pedestrian data (March 2017)
download.file("http://www.pedestrian.melbourne.vic.gov.au/datadownload/March_2017.csv",
  destfile = "data/pedestrian_03_2017.csv")
# Pedestrian_sensor_locations.csv downloaded from https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-sensor-locations/ygaw-6rzq

# DL weather data at cape otway lighthouse station
col_names <- c("ID", "YEAR", "MONTH", "ELEMENT")
col_names <- c(
  col_names, 
  paste0(rep(c("VALUE", "MFLAG", "QFLAG", "SFLAG"), 31), rep(1:31, each = 4))
)
dat <- read_fwf(
  "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/gsn/ASN00090015.dly",
  col_positions = fwf_widths(
    c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), col_names
  )
)
sub_dat <- dat %>% 
  filter(YEAR == 2016) %>% 
  select(ID:ELEMENT, starts_with("VALUE"))

write_csv(sub_dat, path = "data/weather_2016.csv")

# weather <- read_rds("data/au_weather1947.rds")
# melb_weather <- weather %>% 
#   filter(station == "MELBOURNE REGIONAL OFFICE") %>% 
#   mutate(
#     YEAR = year(date),
#     MONTH = month(date),
#     TAVG = (TMIN + TMAX) / 2
#   ) %>% 
#   filter(YEAR != 2015) %>% 
#   group_by(YEAR, MONTH) %>% 
#   summarise(AVERAGE = mean(TAVG, na.rm = TRUE))
#
# write_csv(melb_weather, path = "data/melb_weather.csv")
