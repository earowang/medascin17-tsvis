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
selected_id <- c(1, 10, 18, 19, 24)
sub_ped_ts <- ped_ts[, selected_id]
autoplot(sub_ped_ts, facet = TRUE) + theme_remark()

## ---- ped-long
ped_long <- ped_2017 %>% 
  gather(Sensor_Name, Counts, `State Library`:Southbank) %>% 
  mutate(
    Date_Time = dmy_hms(paste(Date, Hour, "00:00")),
    Date = dmy(Date)
  )
ped_long

## ---- otway-weather
otway_weather <- read_csv("data/weather_2016.csv")
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
  mutate(VALUE = if_else(VALUE == -999.90, NA_integer_, VALUE)) %>% 
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
  mutate(VALUE = if_else(VALUE < -999, NA_integer_, VALUE)) %>% 
  spread(ELEMENT, VALUE) %>% 
  mutate(
    TAVG = TAVG / 10,
    TMAX = TMAX / 10,
    TMIN = TMIN / 10
  )
head(otway_tidy)

## ---- ped-ggplot-1
seleted_names <- colnames(ped_ts)[selected_id]
ped_long %>% 
  filter(Sensor_Name %in% seleted_names) %>% 
  ggplot(aes(x = Date_Time, y = Counts)) +
  geom_line() +
  facet_grid(
    Sensor_Name ~ ., scale = "free_y", 
    labeller = labeller(Sensor_Name = label_wrap_gen(15))
  ) +
  theme_remark()

## ---- ped-ggplot-2
ped_long %>% 
  filter(Sensor_Name %in% seleted_names) %>% 
  ggplot(aes(x = Date_Time, y = Counts, colour = Sensor_Name)) +
  geom_line() +
  facet_grid(
    Sensor_Name ~ ., scale = "free_y", 
    labeller = labeller(Sensor_Name = label_wrap_gen(15))
  ) +
  theme_remark() +
  theme(legend.position = "none")

## ---- southern-x
wday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
sx <- ped_long %>% 
  filter(Sensor_Name == "Southern Cross Station") %>% 
  mutate(
    Wday = wday2(Date, label = TRUE, abbr = FALSE),
    Wday = if_else(Wday %in% wday, "Weekday", "Weekend"),
    Wday = ordered(Wday)
  )
sx

## ---- x-colour
sx %>% 
  ggplot(aes(Hour, Counts, group = Date, colour = Wday)) +
  geom_line() +
  scale_x_continuous(
    breaks = c(0, 5, 8, 12, 17, 22),
    label = c("0:00", "5:00", "8:00", "12:00", "17:00", "22:00")
  ) +
  xlab("Time") +
  theme_remark()

## ---- x-facet
sx %>% 
  ggplot(aes(Hour, Counts, group = Date)) +
  geom_line() +
  facet_wrap(~ Wday, ncol = 2) +
  scale_x_continuous(
    breaks = c(0, 5, 8, 12, 17, 22),
    label = c("0:00", "5:00", "8:00", "12:00", "17:00", "22:00")
  ) +
  xlab("Time") +
  theme_remark()

## ---- x-more
labour <- "Labour Day" # 2013-03-13
adele <- "Adele Day" # 2017-03-18 to 19
# Justin Bieber's gig 2017-03-10
sx_more <- sx %>% 
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
head(sx_more)

## ---- x-more-p
sx_more %>% 
  ggplot(aes(Hour, Counts, group = Date)) +
  geom_line() +
  facet_wrap(~ Wday, ncol = 2) +
  scale_x_continuous(
    breaks = c(0, 5, 8, 12, 17, 22),
    label = c("0:00", "5:00", "8:00", "12:00", "17:00", "22:00")
  ) +
  xlab("Time") +
  theme_remark()

## ---- x-polar
sx_more %>% 
  ggplot(aes(Hour, Counts, group = Date)) +
  geom_line() +
  facet_wrap(~ Wday, ncol = 2) +
  scale_x_continuous(
    breaks = c(0, 5, 8, 12, 17, 22),
    label = c("", "5:00", "8:00", "12:00", "17:00", "22:00")
  ) +
  coord_polar() +
  xlab("Time") +
  theme_remark()

## ---- otway
otway_tidy %>% 
  ggplot() +
  geom_rect(aes(
    xmin = DATE - 1, xmax = DATE + 1, 
    ymin = TMIN, ymax = TMAX),
    colour = "white"
  ) +
  geom_point(aes(x = DATE, y = TAVG))
