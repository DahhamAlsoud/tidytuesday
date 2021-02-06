#load libraries 
library(cfbscrapR)
library(tidyverse)
library(dplyr)
library(gt)
library(extrafont)


#play by play data for the sugar bowl 
sugar.pbp <- cfb_pbp_data(
  year = 2020,
  season_type = "postseason", 
  week = 1, 
  team = "Ohio State", 
  play_type = NULL, 
  epa_wpa = TRUE)

#categorize rush and passes, filter out garbage time
sugar.pbp.plays <- sugar.pbp %>%
  mutate(rush = ifelse(play_type == "Rush", 1, 0),
         pass = ifelse(pass == 1, 1, 0),
         play = case_when(rush == 1 ~ "Run",
                          pass == 1 ~ "Reception"),
         abs_diff = abs(score_diff),
         garbage = ifelse(period == 1 & abs_diff > 43, 1, 
                          ifelse(period == 2 & abs_diff > 37, 1,
                                 ifelse(period == 3 & abs_diff > 27, 1,
                                        ifelse(period == 4 & abs_diff > 22, 1, 0))))) %>% 
  filter(garbage == 0)

#filter for only OSU offense plays 
offense.osu <- sugar.pbp.plays %>% 
  filter(offense_play == "Ohio State") %>% 
  filter(!is.na(play)) 
  
#create WR database 
offense.wr <- offense.osu %>% 
  mutate(player_name = receiver_player_name) %>% 
  filter(pass == 1) %>% 
  group_by(play, player_name) %>% 
  summarize(
    yards.play = mean(yards_gained),
    epa.play = mean(EPA), 
    success.rate = mean(success), 
    total.yards = sum(yards_gained),
    n = n()) %>% 
  arrange(desc(epa.play)) %>% 
  filter(!is.na(player_name))

  
#create RB database 
offense.rb <- offense.osu %>% 
  mutate(player_name = rusher_player_name) %>% 
  filter(rush == 1) %>% 
  group_by(play, player_name) %>% 
  summarize(
    yards.play = mean(yards_gained),
    epa.play = mean(EPA), 
    success.rate = mean(success), 
    total.yards = sum(yards_gained),
    n = n()) %>% 
  arrange(desc(epa.play)) %>% 
  filter(player_name != "TEAM")

#create QB database
offense.qb <- offense.osu %>% 
  mutate(player_name = passer_player_name) %>% 
  filter(pass == 1)

qb.name <- offense.qb %>% 
  group_by(play, player_name) %>% 
  summarize(
    yards.play = mean(yards_gained),
    epa.play = mean(EPA), 
    success.rate = mean(success), 
    total.yards = sum(yards_gained),
    n = n()) 

qb.name[1,1] = "Dropback"

rec.offense <- rbind(offense.wr, offense.rb)
offense.data <- rbind(qb.name,rec.offense)

gt(offense.data) %>% 
  tab_header(title = md("**Ohio State Offense Statistics**"),
             subtitle = ("2021 Allstate Sugar Bowl - Garbage Time Excluded")) %>% 
  tab_source_note(source_note = "Data: @CFB_Data via cfbscrapR | Figure: @juliacat_23") %>%
  fmt_number(
    columns = vars(yards.play, epa.play),
    decimals = 2) %>% 
  fmt_percent(
    columns = vars(success.rate),
    decimals = 2) %>% 
  cols_label(
    player_name = "",
    epa.play = "EPA/Play", 
    success.rate = "Success %", 
    yards.play = "Yards/Play",
    total.yards = "Total Yards", 
    n = "Plays") %>% 
  tab_footnote(
    footnote = "Success is defined as 50% of necessary yardage on first down, 70% on second down, and 100% on third and fourth down.",
    locations = cells_column_labels(
      columns = vars(success.rate))) %>% 
  data_color(
    columns = vars(epa.play),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL)) %>% 
  gt_theme_538()
