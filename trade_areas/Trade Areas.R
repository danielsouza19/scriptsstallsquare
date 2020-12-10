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

######################################## Ditance between common locations and store ###########################
    pin_path <- file.choose()
    pin <-  read_tsv(pin_path)

    home_path <- file.choose()
    home <- read_tsv(home_path) %>% rename(`Hashed Device ID` = 1) %>%  group_by(`Hashed Device ID`) %>%  dplyr::summarise(home_lat = mean(`Common Daytime Lat`), home_long = mean(`Common Daytime Long`))
                
    
    
    store <- pin %>% group_by(`Hashed Device ID`) %>%  dplyr::summarise(store_lat = mean(`Lat of Visit`), store_long = mean(`Lon of Visit`)) %>% 
             inner_join(home, by = "Hashed Device ID") %>%  filter(!is.na(home_lat))   
    
    store <- store %>%  mutate(distance = distHaversine(store[c('store_long', 'store_lat')],store[c('home_long', 'home_lat')])/1000) %>% 
             arrange(distance)
      
    z_store <- select(store, `Hashed Device ID`, home_lat, home_long,distance) %>%  mutate(z_dist = scale(distance)) %>% filter(distance < 13) 
    z <- select(z_store, distance) %>% mutate(distance = distance*1000) 
    row.names(z) <- z_store[["Hashed Device ID"]]
     
    set.seed(123)
    km.res <- kmeans(z, 3, nstart = 1)
    
    z_df <- data.frame(hashed_device_id = names(km.res[["cluster"]]),cluster = km.res[["cluster"]])
    
    row.names(z_df) <- c()
    
    home <-  home %>% inner_join(z_df, by = c("Hashed Device ID" = "hashed_device_id"))
    home <- home %>% mutate(cluster = as.factor(cluster))
    theme_set(theme_bw(16))
    map <- get_googlemap(center = c(lon = 138.6002,lat = -34.94303), markers = data.frame(c(138.6002),c(-34.94303)), scale = 1, zoom = 12)
    
    ggmap(map, extent = 'device') #+  stat_bin2d(
#      aes(x = home_long, y = home_lat, colour = cluster, fill = cluster),
#      size = .5, bins = 60, alpha = 1/2,
#     data = home
#    )
    



