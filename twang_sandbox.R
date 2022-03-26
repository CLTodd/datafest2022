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
  select(player_id, school, event_id, session, event_category, event_time_dbl, avatar_gender, avatar_id, event_type, skill_level_know)


# S5_scores_cleaned data
# range: 1.3-4
# 64 diff players
s5_Scores <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/S5_scores_cleaned.csv")


# merge log subset & s5_Scores
combo <- merge(school, s5_Scores, by="player_id")

# model
model1 <- lm(S5_mean ~ event_time_dbl + weeks, data=combo)
summary(model1)

# convert s5 to wide format
s5_wide <- 
  tidyr::pivot_wider(s5_Scores[s5_Scores$player_id!="NA",], 
                     id_cols = player_id, 
                     names_from = weeks, 
                     values_from = S5_mean, 
                     names_prefix = "week")

# convert school to wide format
school_wide <- 
  tidyr::pivot_wider(school[school$player_id != "NA",], 
                     id_cols = player_id, 
                     names_from = event_id, 
                     values_from = event_time_dbl, 
                     names_prefix = "event")
