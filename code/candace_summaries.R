# Candace
#
# 3/26/2022
#
# Some summary stats, demographics, etc

rm(list=ls())

library(data.table)
library(recipes)
library(dplyr)

setwd("./data")
logsRaw <- readRDS("logsRaw")

# event 602 is when the player commits to their age and gender selection, so that's the event that will give us the demographics

raceKey <- readRDS("raceKey")
genderKey <- readRDS("genderKey")
ageKey <- readRDS("ageKey")

# structure is age, gender
commitAG <-
  logsRaw %>%
  filter(event_id==602) %>%
  select(player_id, event_id, event_time_dbl, data_values) %>%
  arrange(player_id, desc(event_time_dbl)) %>% # Only want their most recent commitment
  group_by(player_id) %>%
  slice_max(order_by = event_time_dbl) %>%
  mutate(ageCode = as.numeric(gsub(pattern=",.", # get the age code
                                   replacement="",
                                   x=data_values))) %>%
  left_join(ageKey, by=c("ageCode"="index")) %>% # get the age
  mutate(genderCode = as.numeric(gsub(pattern=".,", # get the gender code
                                      replacement="",
                                      x=data_values))) %>% 
  left_join(genderKey, by=c("genderCode"="index")) %>% # get the gender
  select(player_id, age, gender, genderCode)
  

commitR <-
  logsRaw %>%
  filter(event_id==603) %>%
  select(player_id, event_id, event_time_dbl, data_values) %>%
  arrange(player_id, desc(event_time_dbl)) %>% # Only want their most recent commitment
  group_by(player_id) %>%
  slice_max(order_by = event_time_dbl) %>%
  mutate(raceCode = as.numeric(data_values)) %>%
  left_join(raceKey, by=c("raceCode"="index")) %>% # get the race
  select(player_id, race, raceCode)

logsDemographics <-
  full_join(commitAG, commitR, by="player_id")

logsDemographics

saveRDS(logsDemographics, "logsDemographics")