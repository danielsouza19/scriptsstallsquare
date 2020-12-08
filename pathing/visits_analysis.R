library(tidyverse)
library(lubridate)
library(esquisse)
library(geosphere)
library("sf")
library(rgeos)
library(ggmap)
library(psycho)
library(factoextra)
library(zoo)

register_google("AIzaSyBy9_aJq6KHrcBPVNoIT-aNWf4KiCLy4vc")

#pull home data
home_path <- file.choose()
home <- read_tsv(home_path)
home <-  home %>% rename(home_lat =`Common Evening Lat`, home_long = `Common Evening Long`, hashed_device_id = 1)

#pull pin data

pin_path <- file.choose()
pin <- read_tsv(pin_path)
pin <- pin  %>% rename(hashed_device_id = 2, time_of_day = 7) %>% mutate(Date = as_date(Date), day_period = ifelse(hms(time_of_day) >= hm("06:00") & hms(time_of_day) < hm("09:00"), "Breakfast",ifelse(hms(time_of_day) >= hm("09:00") & hms(time_of_day) < hm("12:00"), "Mid Morning",ifelse(hms(time_of_day) >= hm("12:00") & hms(time_of_day) < hm("14:00"), "Lunch","Other")))) %>% 
       filter(day_period != "Other")

#pull pathing data
pathing_path <- file.choose()
path <- read_tsv(pathing_path) %>% rename(hashed_device_id = `Hashed Device ID` ) %>% mutate(`Time before appearance in polygon Adj` = abs(abs(`Time before appearance in polygon`) - 1800)) %>% rename(time_of_day = `Local Time of Day`) %>% 
        mutate(day_period = ifelse(hms(time_of_day) >= hm("06:00") & hms(time_of_day) < hm("09:00"), "Breakfast",ifelse(hms(time_of_day) >= hm("09:00") & hms(time_of_day) < hm("12:00"), "Mid Morning",ifelse(hms(time_of_day) >= hm("12:00") & hms(time_of_day) < hm("14:00"), "Lunch","Other")))) %>% 
        filter(day_period != "Other")

path_bv <- path  %>% group_by(hashed_device_id) %>% summarise(time_before_visit = min(`Time before appearance in polygon Adj`))  %>% 
          inner_join(path, by = c("hashed_device_id"="hashed_device_id","time_before_visit" = "Time before appearance in polygon Adj"))

map <- get_googlemap(center = c(lon = 138.6001,lat = 	-34.94284), markers = data.frame(c(138.6001),c(-34.94284)), scale = 1, zoom = 12)
ggmap(map, extent = 'device') + geom_point(data = path_bv, aes(x = `Lon of Observation Point`, y = `Lat of Observation Point`), colour = "blue", alpha = 0.5) + theme_bw()


