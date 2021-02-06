library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(ggthemes)

#Read in week 2 data 
tt_data <- tt_load(2021, week=2)
transit_cost <- tt_data$transit_cost 

#convert country from iso2c to country name 
transit_cost <- transit_cost %>% 
  mutate(
    country = countrycode(country, 
                          origin = "iso2c", 
                          destination = "country.name")) 
#UK read in as "NA", convert NA to "United Kingdom 
transit_cost$country <- as.character(transit_cost$country)
transit_cost$country[is.na(transit_cost$country)] <- "United Kingdom" 

#group each contry by continent 
transit_cost$continent <- countrycode(transit_cost$country, 
                                      origin = "country.name",
                                      destination = "continent")

#construction time 

#convert year from character to date to numeric 
transit_cost$start_year <- as.Date(paste(transit_cost$start_year, 1, 1), '%Y %m %d')
transit_cost$start_year <- as.numeric(format(transit_cost$start_year, "%Y"))
transit_cost$end_year <- as.Date(paste(transit_cost$end_year, 1, 1), '%Y %m %d')
transit_cost$end_year <- as.numeric(format(transit_cost$end_year, "%Y"))

#calculate values
transit_stat <- transit_cost %>% 
  filter(start_year <= 2020) %>% 
  mutate(
    construction_time = end_year - start_year, #project time to complete 
    con_per_length = construction_time/length) %>% #time to complete per rail length 
  filter(!is.na(con_per_length)) #remove NA values

transit_summary <- transit_stat %>% 
  group_by(continent, country) %>% 
  summarize(
    avg.construction.time = mean(construction_time), 
    avg.con.per = mean(con_per_length),
    avg.cost = mean(cost_km_millions)) %>% 
  arrange(avg.con.per)

#plot 
  
transit_summary2 <- transit_summary
transit_summary2$country <- factor(transit_summary2$country,  # Factor levels in decreasing order
                  levels = transit_summary2$country[order(transit_summary2$avg.con.per, decreasing = TRUE)])
  
transit_summary2 %>% 
  filter(continent %in% c("Europe", "Americas")) %>% 
  filter(!country %in% c("Chile", "Panama", "Argentina", "Ecuador", "Peru", "Brazil")) %>% 
  ggplot(aes(x = avg.con.per, y = country)) + 
  geom_bar(stat = "identity", aes(fill = avg.cost), size = 4.5) +
  scale_fill_gradient2(low = "#9cdcec", mid = "#ac9cec", high = "#ec9cdc", midpoint = 650) + 
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + 
  labs(
    title = "Construction and Costs of Country Infrastructure Projects",
    subtitle = "High Capacity Transits Projects From 1982 to 2020 in Europe and North America", 
    caption = "Data: Transit Costs Project | Plot: @juliacat23 | #tidytuesday", 
    fill = "cost per km (USD)") + 
  xlab("Project Completition Time Per Kilometer (years/km)") + 
  ylab("")


  
