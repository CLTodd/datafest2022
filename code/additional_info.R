# Tanya's Sandbox 2
# 3/27/2022

# import packages
library(data.table)
library(dplyr)
library(tidyr)
#library("FastImputation")
#library(missRanger)


# logs data
# 12 diff schools
# 166 diff players
logs <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/logs.csv")

# S5_scores_cleaned data
# range: 1.3-4
# 64 diff players
s5_Scores <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/S5_scores_cleaned.csv")

ggplot(s5_Scores, aes(x=weeks, y=S5_mean)) + 
  geom_point()


final <- logs %>%
  select(player_id, event_time_dbl, minigame_level, skill_level_know, skill_level_priority, 
         skill_level_people, skill_level_refusal, skill_level_me, old_skill_point, 
         new_skill_point, total_points, total_strikes)

# merge log subset & s5_Scores
combo <- merge(final, s5_Scores, by="player_id")

model1 <- lm(S5_mean ~ event_time_dbl + minigame_level + skill_level_know + skill_level_priority + skill_level_people + 
               skill_level_refusal + skill_level_me + total_strikes, data=nona)
summary(model1)