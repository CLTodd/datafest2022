# Tanya's Sandbox
# 3/26/2022

# import packages
library(data.table)
library(dplyr)
library(tidyr)
library("FastImputation")
library(missRanger)


# logs data
# 12 diff schools
# 166 diff players
logs <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/logs.csv")

# S5_scores_cleaned data
# range: 1.3-4
# 64 diff players
s5_Scores <- read.csv("C:/Users/GuaiGuai/Downloads/datafest2022-main/data/S5_scores_cleaned.csv")

###########################################################################################################################

# explore data types
str(logs)

# create sub dataframe w/ only numeric vars of interest
sub <- logs %>% 
  select(event_id, event_time_dbl, stack_id, minigame_level, skill_level_know, skill_level_priority, 
              skill_level_people, skill_level_refusal, skill_level_me, avatar_age, 
         school, minigame_id, old_skill_point, new_skill_point, aa_level_id, chunk_id, piece_id, 
         old_choice_id, choice_id, new_choice_id, total_points, total_strikes)

# create numeric var for gender
sub$avatar_gender <- ifelse(logs$avatar_gender == 'Male', 1, 0)

# check for correlation
cor(sub)

## NOTES:

# pos correlations: 
## event_time_dbl & event_id > event_time_dbl & school

# neg correlations: 
## event_id & school

# final dataframe (consider adding "date" var?)
final <- logs %>%
  select(player_id, event_id, event_time_dbl, stack_id, minigame_level, skill_level_know, skill_level_priority, 
         skill_level_people, skill_level_refusal, skill_level_me, avatar_age, 
         school, minigame_id, old_skill_point, new_skill_point, aa_level_id, chunk_id, piece_id, 
         old_choice_id, choice_id, new_choice_id, total_points, total_strikes)
final$avatar_gender <- ifelse(logs$avatar_gender == 'Male', 1, 0)
final$avatar_id <- unclass(factor(logs$avatar_id))
final$session <- unclass(factor(logs$session))
final$skill_id <- unclass(factor(logs$skill_id))

# merge log subset & s5_Scores
combo <- merge(final, s5_Scores, by="player_id")

# imputate missing values (takes a very long time, even w/ only 1 iteration)
temp <- missRanger(data=combo, 
                   seed=16802, 
                   maxiter=1, 
                   verbose=1)

# save dataset so that we don't need to run the above code again
write.csv(temp, "C:/Users/GuaiGuai/Downloads/datafest2022-main/data/imputed_log_sub.csv", row.names=FALSE)

# initial full model
mod1 <- lm(S5_mean ~., data=temp)
summary(mod1)

mod

###########################################################################################################################

# need to re-do, this is pointless
school <- logs %>%
  group_by(school) %>%
  select(player_id, school, event_id, session, event_category, event_time_dbl, avatar_gender, avatar_id, event_type, skill_level_know)


# merge log subset & s5_Scores
combo <- merge(school, s5_Scores, by="player_id")

# simple model
model1 <- lm(S5_mean ~ event_time_dbl + weeks, data=combo)
summary(model1)

# more complex
model2 <- lm(S5_mean ~ player_id + session + event_category + event_time_dbl + weeks, data=combo)
summary(model2)

# correlation matrix
# cor(insert dataframe here)

# convert s5 to wide format
s5_wide <- 
  tidyr::pivot_wider(s5_Scores[s5_Scores$player_id!="NA",], 
                     id_cols = player_id, 
                     names_from = weeks, 
                     values_from = S5_mean, 
                     names_prefix = "week")

# convert school to wide format
#school_wide <- 
#  tidyr::pivot_wider(school[school$player_id != "NA",], 
#                     id_cols = player_id, 
#                     names_from = session, 
#                     values_from = event_time_dbl, 
#                     names_prefix = "event")

###########################################################################################################################