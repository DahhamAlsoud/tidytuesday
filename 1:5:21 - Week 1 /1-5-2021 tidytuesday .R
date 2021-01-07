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

#load 538 theme 
theme_538 <- function(base_size = 12, base_family = "Gill Sans") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(family = "Gill Sans", size = base_size),
      axis.text = element_text(face = "plain", color = "black", size = base_size),
      axis.title = element_text(face = "bold", size = rel(1.33)),
      axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
      axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
      plot.title = element_text(face = "bold", size = rel(1.67), hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
      plot.caption = element_text(size = 11, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
      plot.background = element_rect(fill = "#f0f0f0", color = NA),
      panel.background = element_rect(fill = "#f0f0f0", color = NA),
      panel.grid.major =  element_line(color = "#d0d0d0"),
      panel.border = element_blank(),
      plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.33), face = "bold"))}
  
transit_plot <- transit_cost %>% 
  select(continent, country, city, start_year, cost_km_millions, line) %>% 
  filter(start_year <= 2020) %>% 
  ggplot(aes(x = start_year, y = cost_km_millions, color = continent)) + 
  geom_point(position = "jitter") + 
  geom_text_repel(
    data = filter(transit_cost, e == "7411"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, e == "8210"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, e == "7329"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, e == "7435"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  geom_text_repel(
    data = filter(transit_cost, e == "7417"),
    aes(label = city),
    force = 1, point.padding = 0.1,
    segment.size = 0.2) +
  theme_538() + 
  labs(x = "Project Start Year", y = "Cost/KM in Millions of USD", 
       title = "Transient Costs Have Slightly Increased Since 1982", 
       subtitle = "The top 5 most expensive projects were in the Americas", 
       caption = "Data: Transit Costs Project | Plot: @juliacat23 | a #TidyTuesday exercise")
 

transit_plot
