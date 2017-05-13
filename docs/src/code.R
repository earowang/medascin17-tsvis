## ---- load-library
library(ggmap)
library(plotly)
library(stringr)
library(forcats)
library(forecast)
library(sugrrants)
library(tidyverse)
library(lubridate)

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
ped_2017 <- read_csv("data/pedestrian_03_2017.csv")
ped_2017

## ---- ped-ts
ped_ts <- ts(ped_2017[, -c(1, 2)], frequency = 24)
sub_ped_ts <- ped_ts[, sample(ncol(ped_ts), 5)]
autoplot(sub_ped_ts, facet = TRUE) + theme_remark()

## ---- ped-long
ped_long <- ped_2017 %>% 
  gather(Sensor_Name, Counts, `State Library`:Southbank) %>% 
  mutate(Date = dmy(Date))
ped_long

## ---- otway-weather
otway_weather <- read_csv("data/weather_2017.csv")
head(otway_weather)

## ---- otway-tidy-1
otway_weather %>% 
  gather(DAY, VALUE, VALUE1:VALUE31)

## ---- otway-tidy-2
otway_weather %>% 
  gather(DAY, VALUE, VALUE1:VALUE31) %>% 
  mutate(
    DAY = str_sub(DAY, start = 6),
    DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-"))
  ) %>% 
  arrange(DATE) %>% 
  select(ID, DATE, ELEMENT, VALUE) %>% 
  filter(!(is.na(DATE)))

## ---- otway-tidy-3
otway_weather %>% 
  gather(DAY, VALUE, VALUE1:VALUE31) %>% 
  mutate(
    DAY = str_sub(DAY, start = 6),
    DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-"))
  ) %>% 
  arrange(DATE) %>% 
  select(ID, DATE, ELEMENT, VALUE) %>% 
  filter(!(is.na(DATE))) %>% 
  spread(ELEMENT, VALUE)

## ---- otway-tidy
otway_tidy <- otway_weather %>% 
  gather(DAY, VALUE, VALUE1:VALUE31) %>% 
  mutate(
    DAY = str_sub(DAY, start = 6),
    DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-"))
  ) %>% 
  arrange(DATE) %>% 
  select(ID, DATE, ELEMENT, VALUE) %>% 
  filter(!(is.na(DATE))) %>% 
  spread(ELEMENT, VALUE) %>% 
  mutate(
    TAVG = TAVG / 10,
    TMAX = TMAX / 10,
    TMIN = TMIN / 10
  )
head(otway_tidy)

## ---- southern-x
wday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
sx <- ped_long %>% 
  filter(Sensor_Name == "Southern Cross Station") %>% 
  mutate(
    Wday = wday2(Date, label = TRUE, abbr = FALSE),
    Wday = if_else(Wday %in% wday, "Weekday", "Weekend"),
    Wday = ordered(Wday)
  )
sx %>% 
  ggplot(aes(Hour, Counts, group = Date)) +
  geom_line() +
  facet_wrap(~ Wday, ncol = 2) +
  scale_x_continuous(
    breaks = c(0, 5, 8, 12, 17, 22),
    label = c("0:00", "5:00", "8:00", "12:00", "17:00", "22:00")
  )

## ---- x-more
labour <- "Labour Day" # 2013-03-13
adele <- "Adele Day" # "2017-03-18" to 19
# justin bieber # 2017-03-10
sx_pull <- sx %>% 
  mutate(
    Wday = fct_expand(Wday, labour, adele),
    Wday = if_else(
      Date == ymd("2017-03-13"), ordered(labour, levels(Wday)), Wday
    ),
    Wday = if_else(
      Date %in% ymd(c("2017-03-18", "2017-03-19")), 
      ordered(adele, levels(Wday)), Wday
    )
  ) 
sx_pull %>% 
  ggplot(aes(Hour, Counts, group = Date)) +
  geom_line() +
  facet_wrap(~ Wday, ncol = 2) +
  scale_x_continuous(
    breaks = c(0, 5, 8, 12, 17, 22),
    label = c("0:00", "5:00", "8:00", "12:00", "17:00", "22:00")
  )

## ---- x-polar
sx_pull %>% 
  ggplot(aes(Hour, Counts, group = Date)) +
  geom_line() +
  facet_wrap(~ Wday, ncol = 2) +
  scale_x_continuous(
    breaks = c(5, 8, 12, 17, 22),
    label = c("5:00", "8:00", "12:00", "17:00", "22:00")
  ) +
  coord_polar()
