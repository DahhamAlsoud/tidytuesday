library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(bbplot)


netflix <- read_csv("NetflixViewingHistory (1).csv",
               col_types = cols(Date = col_date(format = "%D")))

netflix <- netflix %>% 
  separate(Title, sep = ": ", into = c("Name", "Season", "Episode"))

tv.show <- netflix %>% #only focusing on tv shows
  filter(!is.na(Episode)) %>%  
  filter(Date >= "2018-01-01") %>% 
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
  scale_fill_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) + 
  geom_bar(position="stack", stat="identity") + 
  coord_flip() + 
  bbc_style() + 
  theme(axis.text.x = element_text(margin=margin(t = 7, b = 10)), 
        legend.position = "right", 
        axis.title.x = element_text(family = font, size = 20, color = "#222222")) + 
  labs(
    title = "Top 10 Binged TV Shows, 2018 - 2021", 
    subtitle = "shows where I watched at least 3 or more episodes on a single day", 
    fill = "Year",
    y = "Total Number of Episodes Watched", 
    x = "")

ggsave("netflix_plot.png", width = 16, height = 9, units = "in")


binge.tv.show %>% 
  ggplot() +
  geom_col(aes(reorder(Name, episodes_n), y=episodes_n), fill = "aquamarine4") +
  coord_flip() + 
  ggtitle("Top 10 binge-watched series") +
  labs(y="Total episode", x="My top 10 series") +
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank())

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
  ggtitle("Number of episodes per day") +
  labs(y="number of episode", x="Date") +
  bbc_style()

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
       subtitle = "Number of episodes watched per weekday from 2018 to 2021") +
  theme_minimal() + 
  theme( 
    plot.title = element_text(family = font, size = 22, face = "bold", color = "#222222"), 
    plot.title.position = "plot", 
    plot.subtitle = element_text(family = font, size = 20, margin = ggplot2::margin(9, 0, 9, 0)), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = font, size = 14))

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
       subtitle = "Number of episodes watched per month from 2018 to 2021") +
  theme_minimal() + 
  theme( 
    plot.title = element_text(family = font, size = 22, face = "bold", color = "#222222"), 
    plot.title.position = "plot", 
    plot.subtitle = element_text(family = font, size = 20, margin = ggplot2::margin(9, 0, 9, 0)), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = font, size = 14))

ggsave("months.png",  width = 12, height = 9, units = "in")

nextflix.cat <- read.csv("netflix_titles.csv") %>% 
  separate('listed_in', c('cat_1', 'cat_2', 'cat_3'), sep = ',')

tv.show.cat <- tv.show %>% 
  inner_join(nextflix.cat, by = c('Name'='title')) 

tv.show.cat %>% 
  select(Name, cat_2) %>% 
  group_by(Name) %>% 
  summarise(
    cat = first(cat_2)
  ) %>% 
  filter(!is.na(cat)) %>% 
  group_by(cat) %>% 
  summarise(
    n = n()
  ) %>% 
  ggplot() +
  geom_col(aes(cat, n), fill="sienna1") +
  coord_flip()+
  ggtitle("Series by categories") +
  labs(y="nb of series", x="Categories") +
  theme_minimal()
