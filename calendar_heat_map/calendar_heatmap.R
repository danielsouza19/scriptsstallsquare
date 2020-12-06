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

pin <- read_tsv("data/pin.tsv")



daily_payments <- payments %>% group_by(payment_date) %>% summarise(ValueCol = n()) %>% filter(payment_date < as_date('2020-11-26'))
#daily_activeness <- hourly_activeness %>% filter(membership_type_id == 120) %>%  group_by(date) %>% summarise(ValueCol = mean(daily_stickness)) %>% ungroup()


#df <-  df %>% left_join(daily_activeness, by = c("DateCol" = "date"))

df <-  df %>% left_join(daily_payments, by = c("DateCol" = "payment_date")) %>% mutate(ValueCol = as.integer(ValueCol))

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
                       values = scales::rescale(c(-1, -0.05, 0, 0.05, 1)),
                       name = "Trial Payments",
                       guide = guide_colorbar(title.position = "top", 
                                              direction = "horizontal")) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Trial Payments Heatmap")