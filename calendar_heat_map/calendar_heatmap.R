library(rvest)
library(tidyverse)
library(lubridate)
library(odbc)
library(RMySQL)
library(esquisse)
library(data.table)

Sys.setlocale("LC_ALL","English")

df  <-  tibble(
  DateCol = seq(
    dmy("01/01/2020"),
    dmy("30/11/2020"),
    "days"
  ))

pin_path <- file.choose()

pin <- read_tsv(pin_path)
pin <- pin %>% rename(hashed_device_id = 2, time_of_day = 7) %>% mutate(Date = as_date(Date), day_period = ifelse(hms(time_of_day) >= hm("06:00") & hms(time_of_day) < hm("09:00"), "Breakfast",ifelse(hms(time_of_day) >= hm("09:00") & hms(time_of_day) < hm("12:00"), "Mid Morning",ifelse(hms(time_of_day) >= hm("12:00") & hms(time_of_day) < hm("14:00"), "Lunch","Other"))))

#summarise the columns -- make specific stuff for each site clear, to be easy to change later

#colocar NA = 0? Talvez a escala fique mais bem distribuída
visits <- pin %>% filter(day_period == "Lunch") %>%  group_by(Date) %>% summarise(ValueCol = n_distinct(hashed_device_id))

df <-  df %>% left_join(visits, by = c("DateCol" = "Date")) %>% mutate(ValueCol = as.integer(ValueCol))

dfPlot <- df %>% 
  mutate(weekday = lubridate::wday(DateCol, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
         month = lubridate::month(DateCol, label = T),
         date = lubridate::yday(DateCol),
         week = lubridate::epiweek(DateCol))

# isoweek makes the last week of the year as week 1, so need to change that to week 53 for the plot
dfPlot$week[dfPlot$month=="Dec" & dfPlot$week ==1] = 53 

dfPlot <- dfPlot %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))

dfPlot %>%
  ggplot(aes(weekday,-week, fill = ValueCol)) +
  geom_tile(colour = "white")  + 
  geom_text(aes(label = day(DateCol)), size = 2.5, color = "black") +
  theme(aspect.ratio = 1/2,
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) +
  scale_fill_gradientn(colours = c("red", "white", "#6b9235"),
                       values = scales::rescale(c(-1, -0.08, 0, 0.02, 1)),
                       name = "Number of Visitors",
                       guide = guide_colorbar(title.position = "top", 
                                              direction = "horizontal")) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Visitors Heatmap")