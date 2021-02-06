# Load libraries, data ------------------------------------------------
library(ggplot2)
library(tidyverse)
library(espnscrapeR)
library(gt)
library(bbplot)

us.data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv") #us vaccine data
us.policy <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
# Clean up data -------------------------------------------------------
us.policy.clean <- us.policy %>% 
  filter(!RegionName %in% c("", " ")) %>% 
  filter(RegionName != "Virgin Islands") %>% 
  select(RegionName, StringencyIndex) %>% 
  filter(!is.na(StringencyIndex)) %>% 
  group_by(RegionName) %>% 
  summarize(
    avg_string = mean(StringencyIndex)) 
us.state.data <- us.data %>% 
  filter(! location %in% c("American Samoa", "Bureau of Prisons", "Dept of Defense", "Federated States of Micronesia", 
                           "Guam", "Indian Health Svc", "Long Term Care", "Marshall Islands", "Northern Mariana Islands",
                           "Puerto Rico", "Republic of Palau", "United States", "Veterans Health", "Virgin Islands")) %>% 
  filter(!is.na(share_doses_used)) %>% 
  filter(!is.na(daily_vaccinations_per_million)) %>%  
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>% 
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>% 
  group_by(location) %>% 
  summarize(
    avg_daily_rate = mean(daily_vaccinations_per_million),
    dose_used = max(share_doses_used),
    cum_vac = max(total_vaccinations_per_hundred),
    cum_ppl_vac = max(people_vaccinated_per_hundred),
    cum_ppl_fully_vac = max(people_fully_vaccinated_per_hundred)) %>% 
  mutate(
    zscore = (cum_ppl_fully_vac - mean(cum_ppl_fully_vac))/sd(cum_ppl_fully_vac))
us.state.data[33, "location"] = "New York"

com.state <- us.state.data %>% 
  inner_join(us.policy.clean, by = c("location" = "RegionName"))

com.state %>%
  ggplot(aes(x = zscore, y = reorder(location, zscore))) +
  geom_col(aes(fill = if_else(zscore >= 0, "#2c7bb6", "#d7181c"))) +
  scale_fill_identity() 
  