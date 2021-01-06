library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(ggmap)
library(RColorBrewer)
library(bbplot)
library(ggthemes)
library(ggrepel)

#Read in week 2 data 
tt_data <- tt_load(2021, week=2)
transit_cost <- tt_data$transit_cost 

#convert country from iso2c to country name 
transit_cost <- transit_cost %>% 
  mutate(
    country = countrycode(country, 
                          origin = "iso2c", 
                          destination = "country.name")) 
#UK read in as "NA", convert NA to "Great Britian 
transit_cost$country <- as.character(transit_cost$country)
transit_cost$country[is.na(transit_cost$country)] <- "Great Britian" 

#group each contry by continent 
transit_cost$continent <- countrycode(transit_cost$country, 
                                      origin = "country.name",
                                      destination = "continent")
#replace UK "NA" as "Europe" 
transit_cost$continent <- as.character(transit_cost$continent)
transit_cost$continent[is.na(transit_cost$continent)] <- "Europe" 

#identify and select variables for anaylsis 
#     Continent 
#     Country
#     City 
#     Start Year 
#     Cost/Km in Millions of USD 

transit_plot <- transit_cost %>% 
  select(continent, country, city, start_year, cost_km_millions, line) %>% 
  filter(start_year <= 2020) %>% 
  ggplot(aes(x = start_year, y = cost_km_millions, color = continent)) + 
  geom_point(position = "jitter") + 
  geom_text_repel(
    data = filter(transit_cost, line == "East Side Access"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, line == "Second Avenue Phase 2"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, line == "Circle Line Stage 6"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, line == "City Rail Link"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  theme_fivethirtyeight() + 
  labs(x = "Project Start Year", y = "Cost/KM in Millions of USD", 
       title = "Transient Costs Have Slightly Increased Since 1982", 
       subtitle = "A #tidytuesday exercise", 
       caption = "Data: Transit Costs Project | Plot: @juliacat23")
 

transit_plot
