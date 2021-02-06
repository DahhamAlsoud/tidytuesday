library(tidyverse)
library(tidytuesdayR)
library(ggridges)
library(paletteer)
library(extrafont)
library(ggthemr)


#load data
tt <- tt_load("2021-01-12")
artists <- tt$artists
artwork <- tt$artwork

#clean up data 
artists <- artists %>% 
  select(name, gender, yearOfBirth, yearOfDeath, placeOfBirth, placeOfDeath) 
artwork <- artwork %>% 
  select(artist, medium, year)
art_data <- artwork %>%
  inner_join(artists, by = c( "artist" = "name"))

#recoding medium categories
art_medium <- art_data %>% 
  filter(!is.na(medium)) %>% 
  mutate(
    medium = str_extract(medium, "^([^ ]+)"), #command to match the first word of the string before the space
    medium = str_replace(medium, ",", " "), #any commas in the string will be replaced with a string
    medium = fct_recode(medium, "Paper/Engraving" = "Line", #fct_recode() renames the factors
                        "Paper/Engraving" = "Engraving", 
                        "Pen/Ink" = "Pen", 
                        "Pen/Ink" = "Ink"), 
    medium = str_trim(medium)) %>% 
  filter(!is.na(year)) 

art_medium_check <- art_medium %>% 
  filter(year >= 1800) %>% 
  group_by(medium) %>% 
  summarize(
    n = n()) %>% 
  arrange(desc(n))

art_medium_cnt <- art_medium %>% 
  group_by(year, medium) %>% 
  select(medium, year) %>% 
  filter(year >= 1800) %>% 
  arrange(year) %>% 
  filter(medium %in% c("Graphite", 
                       "Oil", 
                       "Screenprint", 
                       "Lithograph", 
                       "Etching", 
                       "Watercolour", 
                       "Photograph", 
                       "Pen/Ink", 
                       "Paper/Engraving", 
                       "Gouache"))

#graph 
art_plot <- art_medium_cnt %>% 
  ggplot(aes(x = year, y = fct_reorder(medium,year), fill = medium)) + 
  geom_density_ridges() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(face = "bold")) + 
  labs(title = "Popular Mediums in the Tate Art Museum",
       subtitle = "Tate is an institution that houses the United Kingdom's national collection of British art, and international modern and contemporary art. The frequencies of the top 10 most 
commonly used mediums in Tate's collection produced since 1800 are displayed below",
       x = "", 
       y = "") 

art_plot + 
 
 










