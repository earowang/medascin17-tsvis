## ---- load-library
library(forecast)
library(tidyverse)
library(ggmap)

## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
  theme(
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 16), 
    axis.title = element_text(size = 16)
  )
}
theme_set(theme_remark())

## ---- ped-loc
ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")
ped_loc %>% 
  select(`Sensor ID`, `Sensor Description`, Longitude, Latitude)

## ---- ped-map
melb_map <- get_map(
  location = c(min(ped_loc$Longitude), min(ped_loc$Latitude),
               max(ped_loc$Longitude), max(ped_loc$Latitude)),
  zoom = 14
)

ggmap(melb_map) +
  geom_point(data = ped_loc, aes(x = Longitude, y = Latitude),
    colour = "#756bb1", alpha = 0.8, size = 5) +
  xlab("Longitude") +
  ylab("Latitude")

## ---- ped-wide
ped_2017 <- read_csv("http://www.pedestrian.melbourne.vic.gov.au/datadownload/March_2017.csv")
ped_2017

## ---- ped-ts
ped_ts <- ts(ped_2017[, -c(1, 2)], frequency = 24)
sub_ped_ts <- ped_ts[, sample(ncol(ped_ts), 5)]
autoplot(sub_ped_ts, facet = TRUE)

## ---- billboard
bb <- read_csv("data/billboard.csv")
