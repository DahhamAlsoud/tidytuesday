library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(bbplot)

# --Credits to haoido on Kaggle for tutorial for code, 
# -- I tried to write to run easier and modified it to include some extra information

# Read in Netflix viewing history
netflix <- read_csv("NetflixViewingHistory (1).csv",
               col_types = cols(Date = col_date(format = "%D")))

#Extract Name, Season, and Episode into seperate columns 
netflix <- netflix %>% 
  separate(Title, sep = ": ", into = c("Name", "Season", "Episode"))

tv.show <- netflix %>% #only focusing on tv shows
  filter(!is.na(Episode)) %>%  
  filter(Date >= "2018-01-01") %>% #2018 to present
  filter(!Name %in% c("Better Call Saul")) #my ex boyfriend used my account once to watch Better Call Saul

# binge watched shows (4 or more episodes in a day)

binge.tv.show <- tv.show %>% 
  group_by(Name, Date) %>% #only focusing on date and name
  summarize(
    episodes_n = n()) %>% 
  filter(episodes_n >= 2) %>% 
  group_by(Name) %>% 
  summarize(
    days_n = n(),
    episodes_n = sum(episodes_n)) %>% 
  arrange(desc(episodes_n)) %>% 
  head(10)

binge.tv <- tv.show %>% 
  filter(Name %in% c("That '70s Show", "Forensic Files", "BoJack Horseman", "The Good Place", "Big Mouth", "Dark", "Sex Education", "I Am Not Okay With This", "Parks and Recreation", "How to Sell Drugs Online (Fast)")) %>% 
  group_by(Name, Date) %>% 
  summarize(
    episodes_n = n(), 
    year = lubridate::year(Date)) %>% 
  group_by(Name, year) %>% 
  summarize(
    episodes = n())

font <- "Helvetica"

binge.tv %>% 
  ggplot(aes(x = reorder(Name, episodes, sum), y = episodes, fill = as.factor(desc(year)))) + 
  scale_fill_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600"), labels = c("2021", "2020", "2019", "2018")) + 
  geom_bar(position="stack", stat="identity") + 
  coord_flip() + 
  bbc_style() + 
  theme(axis.text.x = element_text(margin=margin(t = 7, b = 10)), 
        legend.position = "bottom", 
        axis.title.x = element_text(family = font, size = 20, color = "#222222"),
        plot.caption = element_text(family = font, size = 12, color = "#222222")) + 
  labs(
    title = "Top 10 Binged TV Shows, 2018 - 2021", 
    subtitle = "Shows where I watched at least 3 or more episodes on a single day", 
    caption = "Data: Netflix© Viewing History | Plot: juliacat23",
    fill = "Year",
    y = "Total Number of Episodes Watched", 
    x = "")

ggsave("netflix_plot.png", width = 16, height = 9, units = "in")


# number of episodes per day 

daily.ep <- tv.show %>% 
  group_by(Name, Date) %>% 
  summarize(
    episodes_n = n(), 
    year = lubridate::year(Date)) %>% 
  group_by(Name, Date, year) %>% 
  summarize(
    episodes = n()) %>% 
  arrange(desc(Date))
head(daily.ep)

daily.ep %>% 
  ggplot(aes(x = Date, y = episodes)) + 
  geom_col(aes(fill = as.factor(year))) + 
  bbc_style() + 
  annotate(geom = "curve", x = as.Date("2018-06-28"), y = 9.5, xend = as.Date("2018-05-15"), yend = 9, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) + 
  annotate(geom = "text", x = as.Date("2018-06-29"), y = 9.5, label = "That '70s Show", hjust = "left", size = 5) + 
  annotate(geom = "curve", x = as.Date("2020-08-02"), y = 9.3, xend = as.Date("2020-12-04"), yend = 7.9, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) + 
  annotate(geom = "text", x = as.Date("2020-11-20"), y = 9.5, label = "Season 4 of Big Mouth Released", hjust = "right", size = 5) + 
  annotate(geom = "curve", x = as.Date("2020-05-20"), y =6.2 , xend = as.Date("2020-02-01"), yend = 6.5, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) + 
  annotate(geom = "text", x = as.Date("2020-04-01"), y = 6, label = "BoJack Horseman", hjust = "left", size = 5) + 
  annotate(geom = "text", x = as.Date("2020-04-20"), y = 5.7, label = "Final Season", hjust = "left", size = 5) + 
  theme(axis.text.x = element_text(margin=margin(t = 7, b = 10)), 
        legend.position = "none", 
        axis.title.y = element_text(family = font, size = 16, color = "#222222"),
        plot.caption = element_text(family = font, size = 12, color = "#222222")) +
  labs(
    title = "Number of Episodes Per Day", 
    subtitle = "Daily episodes watched from 2018 to 2021", 
    caption = "Data: Netflix© Viewing History | Plot: juliacat23",
    y = "Episodes Watched", 
    x = "")

ggsave("daily_ep.png", width = 16, height = 9, units = "in")

# Viewing by Month/Day 

netflix.week <- tv.show %>% 
  mutate(Month = month(Date, label = T),
         Year = year(Date),
         Weekdays = wday(Date, label = T, week_start = getOption("lubridate.week.start", 1))) 

netflix.week %>% 
  group_by(Weekdays) %>%
  summarise(
    n = n()) %>% 
  ggplot(aes(x = Weekdays, y = n)) +
  geom_col(aes(fill = if_else(Weekdays == "Sat", "#1380A1", "#dddddd"))) + 
  scale_fill_identity() + 
  coord_polar() +
  labs(x="", y="",
       title = "I Watch The Most TV Episodes on Saturdays", 
       subtitle = "Number of episodes watched per weekday from 2018 to 2021",
       caption = "Data: Netflix© Viewing History | Plot: juliacat23") +
  theme_minimal() + 
  theme( 
    plot.title = element_text(family = font, size = 22, face = "bold", color = "#222222"), 
    plot.title.position = "plot", 
    plot.subtitle = element_text(family = font, size = 20, margin = ggplot2::margin(9, 0, 9, 0)), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = font, size = 14), 
    plot.caption = element_text(family = font, size = 12, color = "#222222"))

ggsave("weekdays.png",  width = 12, height = 9, units = "in")

netflix.week %>% 
  group_by(Month) %>%
  summarise(
    n = n()) %>% 
  ggplot(aes(x = Month, y = n)) +
  geom_col(aes(fill = if_else(Month == "May", "#1380A1", "#dddddd"))) + 
  scale_fill_identity() + 
  coord_polar() +
  labs(x="", y="",
       title = "The Majority of Episodes are Watched in May", 
       subtitle = "Number of episodes watched per month from 2018 to 2021",
       caption = "Data: Netflix© Viewing History | Plot: juliacat23") +
  theme_minimal() + 
  theme( 
    plot.title = element_text(family = font, size = 22, face = "bold", color = "#222222"), 
    plot.title.position = "plot", 
    plot.subtitle = element_text(family = font, size = 20, margin = ggplot2::margin(9, 0, 9, 0)), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = font, size = 14),
    plot.caption = element_text(family = font, size = 12, color = "#222222"))

ggsave("months.png",  width = 12, height = 9, units = "in")


