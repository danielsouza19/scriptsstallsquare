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

alberton_home <-  read_tsv("data/otr-alberton/home.tsv") %>% rename(home_lat = `Common Evening Lat`, home_long = `Common Evening Long`) %>% mutate(site = "Alberton") %>% select(device_id = `Device Id`, home_lat, home_long, site)
ws_home <- read_tsv("data/otr-woodville-south/home.tsv") %>% rename(home_lat = `Common Evening Lat`, home_long = `Common Evening Long`) %>% mutate(site = "Woodville South") %>% select(device_id = `Hashed Device Id`, home_lat, home_long, site)
ww_home <- read_tsv("data/otr-woodville-west/home.tsv") %>% rename(home_lat = `Common Evening Lat`, home_long = `Common Evening Long`) %>% mutate(site = "Woodville West") %>% select(device_id = `Hashed Device Id`, home_lat, home_long, site)
prospect_home <- read_tsv("data/otr-prospect/home.tsv") %>% rename(home_lat = `Common Evening Lat`, home_long = `Common Evening Long`) %>% mutate(site = "Prospect") %>% select(device_id = `Hashed Device Id`, home_lat, home_long,site)
seaton_home <- read_tsv("data/otr-seaton/home.tsv") %>% rename(home_lat = `Common Evening Lat`, home_long = `Common Evening Long`) %>% mutate(site = "Seaton") %>% select(device_id = `Hashed Device Id`, home_lat, home_long,site)


ws_pin <- read_tsv("data/otr-woodville-south/pin.tsv") %>% mutate(site = "Woodville South")
ww_pin <- read_tsv("data/otr-woodville-west/pin.tsv") %>% mutate(site = "Woodville West")
prospect_pin <- read_tsv("data/otr-prospect/pin.tsv") %>% mutate(site = "Prospect")
seaton_pin <- read_tsv("data/otr-seaton/pin.tsv") %>% mutate(site = "Seaton")
alberton_pin <- read_tsv("data/otr-alberton/pin.tsv")%>% mutate(site = "Alberton")

home <- rbind(alberton_home, ws_home, ww_home, seaton_home)
pin <- rbind(alberton_pin, ws_pin, ww_pin, seaton_pin) %>% filter(Date < as_date('2020-04-03'))

visits <- pin %>% inner_join(home, by = c("Hashed Device ID"="device_id", "site"="site"))
map <- get_googlemap(center = c(lon = 138.5291,lat = 	-34.87735), markers = data.frame(c(138.5132,138.5315,	138.5141,	138.5291,138.506846),c(-34.85928,-34.88486,-34.90209,-34.87735,-34.8741711)), scale = 1, zoom = 13)
ggmap(map, extent = 'device') + coord_cartesian()+ stat_binhex(data = visits, aes(x = home_long, y = home_lat), alpha = 0.6, color = "darkgray", bins = 10)+
  scale_fill_gradient(high = "darkred", low = "orange") + facet_wrap(vars(`Day of Week`)) + theme_bw() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())

visits_count <- visits %>% group_by(`Hashed Device ID`, `Day of Week`) %>% summarise(visits_count = n())

write.csv2(visits, "hehehe.csv")