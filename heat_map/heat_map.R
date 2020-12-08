library(tidyverse)
library(lubridate)
library(esquisse)
library(geosphere)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library(rgeos)
library(ggmap)
library(psycho)
library(factoextra)

register_google("AIzaSyBy9_aJq6KHrcBPVNoIT-aNWf4KiCLy4vc")


# file picker pra encontrar o arquivo "home". É o CEL/CDL.
home_path <- file.choose()
home <- read_tsv(home_path)
home <-  home %>% rename(home_lat =`Common Evening Lat`, home_long = `Common Evening Long`, hashed_device_id = 1)

# file picker pra encontrar o arquivo pin. É o CEL/CDL.
pin_path <- file.choose()
pin <- read_tsv(pin_path)

pin <- pin  %>% rename(hashed_device_id = 2, time_of_day = 7) %>% mutate(Date = as_date(Date), day_period = ifelse(hms(time_of_day) >= hm("06:00") & hms(time_of_day) < hm("09:00"), "Breakfast",ifelse(hms(time_of_day) >= hm("09:00") & hms(time_of_day) < hm("12:00"), "Mid Morning",ifelse(hms(time_of_day) >= hm("12:00") & hms(time_of_day) < hm("14:00"), "Lunch","Other")))) %>% 
      filter(day_period != "Other")
  
#definir coordenadas do site, pra centralizar e colocar markers


#ordenar corretamente os dias da semana
visits <- pin %>% inner_join(home, by = c("hashed_device_id"="hashed_device_id"))
map <- get_googlemap(center = c(lon = 138.6002,lat = 	-34.94303), markers = data.frame(c(138.6002),c(-34.94303)), scale = 1, zoom = 13)
ggmap(map, extent = 'device') + coord_cartesian()+ stat_binhex(data = visits, aes(x = home_long, y = home_lat), alpha = 0.6, color = "darkgray", bins = 10)+
  scale_fill_gradient(high = "darkred", low = "orange") + facet_wrap(vars(`Day of Week`)) + theme_bw() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())

