library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(bbplot)

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
  select(continent, country, city,length, start_year, cost_km_millions) %>%
  ggplot(aes(x = continent, y = cost_km_millions)) + 
  geom_boxplot()
  

transit_plot
