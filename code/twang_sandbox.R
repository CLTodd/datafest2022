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

# convert s5 to wide format
s5_wide <- 
  tidyr::pivot_wider(s5_Scores[s5_Scores$player_id!="NA",], 
                     id_cols = player_id, 
                     names_from = weeks, 
                     values_from = S5_mean, 
                     names_prefix = "week")

# explore data types
str(logs)

# final dataframe (consider adding "date" var?) - summarize data based on weeks w/ survey score wide format
#final <- logs %>%
#  select(player_id, event_id, event_time_dbl, stack_id, minigame_level, skill_level_know, skill_level_priority, 
#         skill_level_people, skill_level_refusal, skill_level_me, avatar_age, 
#         school, minigame_id, old_skill_point, new_skill_point, aa_level_id, chunk_id, piece_id, 
#         old_choice_id, choice_id, new_choice_id, total_points, total_strikes)

#final$avatar_gender <- ifelse(logs$avatar_gender == 'Male', 1, 0)
#final$avatar_id <- unclass(factor(logs$avatar_id))
#final$session <- unclass(factor(logs$session))
#final$skill_id <- unclass(factor(logs$skill_id))

final <- logs %>%
  select(player_id, event_time_dbl, minigame_level, skill_level_know, skill_level_priority, 
         skill_level_people, skill_level_refusal, skill_level_me, old_skill_point, 
         new_skill_point, total_points, total_strikes)


# work w/ date var
pacman::p_load(
  lubridate, aweek, zoo, tidyverse, rio
)

final$date <- as.Date(logs$date)

final[, c(2:12)] <- sapply(final[, c(2:12)], as.numeric)

# work w/ only one player for now
one <- filter(final, player_id=='6427001')

#drop <- c("player_id")
#one2 <- one[,!(names(one) %in% drop)]
a <- filter(one, date=="2013-03-14")
a <- colSums(a[, c(2:12)], na.rm=TRUE)
b <- filter(one, date==one[1, "date"]+days(21))
b <- colSums(b[, c(2:12)], na.rm=TRUE)
c <- filter(one, date==one[1, "date"]+days(56))
c <- colSums(c[, c(2:12)], na.rm=TRUE)

all$player_id <- c('6427001')
all$event_time_dbl0 <- a[1]
all$minigame_level0 <- a[2]
all$skill_level_know0 <- a[3]
all$skill_level_priority0 <- a[4]
all$skill_level_people0 <- a[5]
all$skill_level_refusal0 <- a[6]
all$skill_level_me0 <- a[7]
all$



# merge log subset & s5_Scores
combo <- merge(all, s5_wide, by="player_id")

#########################################################################################################################

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

# imputate missing values (takes a very long time, even w/ only 1 iteration)
#temp <- missRanger(data=combo, 
#                   seed=16802, 
#                   maxiter=1, 
#                   verbose=1)

# save dataset so that we don't need to run the above code again
#write.csv(temp, "C:/Users/GuaiGuai/Downloads/datafest2022-main/data/imputed_log_sub.csv", row.names=FALSE)

#########################################################################################################################

# re-analyze correlations
cor(temp)

# initial full model (8% adj r-squared)
mod1 <- lm(S5_mean ~., data=temp)
summary(mod1)

# more models (adj r-squared getting progressively smaller)
mod2 <- lm(S5_mean ~  event_time_dbl + stack_id + minigame_level + avatar_age + 
             school + chunk_id + piece_id + total_strikes + avatar_gender + avatar_id + weeks, data=temp)
summary(mod2)

mod3 <- lm(S5_mean ~ player_id + event_id + event_time_dbl + stack_id + minigame_level + avatar_age + school + 
             minigame_id + old_skill_point + aa_level_id + chunk_id + piece_id + old_choice_id + choice_id + total_points + 
             total_strikes + avatar_gender + avatar_id + session + skill_id + weeks, data=temp)
summary(mod3)

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


# convert school to wide format
#school_wide <- 
#  tidyr::pivot_wider(school[school$player_id != "NA",], 
#                     id_cols = player_id, 
#                     names_from = session, 
#                     values_from = event_time_dbl, 
#                     names_prefix = "event")

###########################################################################################################################