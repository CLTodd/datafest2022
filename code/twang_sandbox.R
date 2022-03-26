# Tanya's Sandbox
# 3/26/2022

# import packages
library(data.table)
library(dplyr)
library(tidyr)


# logs data
# 12 diff schools
# 166 diff players
logs <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/logs.csv")

# need to re-do, this is pointless
school <- logs %>%
  group_by(school) %>%
  select(player_id, school, session, event_category, event_time_dbl, avatar_gender, avatar_id, event_type, skill_level_know)


# S5_scores_cleaned data
# range: 1.3-4
# 64 diff players
s5_Scores <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/S5_scores_cleaned.csv")


# merge(s5 scores, logs by playerid)
combo <- merge(school, s5_Scores, by="player_id")
