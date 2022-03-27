######
# Candace
# 3/25/22
#
# This may get changed later, this is basically a sandbox

setwd("./data") # does this work for anyone else?
library(data.table)
library(naniar)
library(dplyr)
library(tidyr)

############################## log data

# Only needed to do this once                  
#logsRaw <- fread("C:/Users/Candace/Downloads/logs.csv")
#saveRDS(logsRaw, "logsRaw")

# setwd("./data")
logsRaw <- readRDS(logsRaw)

# Replace "NA" strings with NA values
# it's weird that there's already a mix of NA strings and values in here
# Is that worth worrying about?
# logs <- replace_with_na_all(logsRaw, ~.x=="NA") 
# Actually this literally takes forever don't run it :(

# List of schools in the log
logsSchools <-
  logsRaw %>%
  group_by(school) %>%
  summarise(count=n()) %>%
  select(school)

# Number of times each player appears in the log
logsPlayers <-
  logsRaw %>%
  group_by(player_id) %>%
  summarise(count=n())

# Number of players at each school
logsSchoolsCounts <-
  logsRaw %>%
  group_by(player_id, school) %>%
  summarise(freq=1) %>%
  group_by(school) %>%
  summarise(count=n())


################################## s5 data

s5Raw <- fread("./S5_scores_cleaned.csv")

# Replace "NA" strings with NA values
s5 <- replace_with_na_all(s5Raw, ~.x=="NA")


# s5 wide format, ever player is one row now
s5wide <- 
  tidyr::pivot_wider(s5Raw[s5Raw$player_id!="NA",], 
                     id_cols = player_id, 
                     names_from = weeks, 
                     values_from = S5_mean, 
                     names_prefix = "week")

saveRDS(s5wide, file="s5wide")

# number of s5 entries each player has
s5Players <-
  s5 %>%
  group_by(player_id) %>%
  summarise(count=n())


############# Challenge #1

# players in both the log data and the s5 data
sum(s5Players$player_id %in% logsPlayers$player_id)

# s5 wide with no NA
s5wideNoNa <-
  s5wide %>%
  filter(!is.na(week0), 
         !is.na(week3), 
         !is.na(week6), 
         !is.na(week12), 
         !is.na(week24),
         !is.na(player_id))

# convert player ids to numeric before trying to join
s5wideNoNa$player_id <- as.numeric(s5wideNoNa$player_id)

# joining
left <- left_join(s5wideNoNa, logsPlayers, by="player_id")
head(left)
nrow(left)


temp <- data.table(left$player_id)



fwrite(temp, file="players.csv")



#### Challenge #3
skills <- readRDS("minigameKey")
dem <- readRDS("logsDemographics")
logs <- readRDS("logsRaw")

# 
temp <- logsRaw[,c(2, 11, 16:20)]
  
temp2 <-
temp %>%
  left_join(dem, by="player_id") %>%
  filter(event_time_dbl<=50e3, 
         !is.na(skill_level_know),
         !is.na(skill_level_priority),
         !is.na(skill_level_people),
         !is.na(skill_level_refusal),
         !is.na(skill_level_me)) %>%
  arrange(player_id, desc(event_time_dbl)) %>% # Only want their latest entries
  group_by(player_id) %>%
  slice_max(order_by = event_time_dbl)

summary(temp2)

library(gridExtra)  
  
ggplot(temp2) +
  geom_point(aes(x=event_time_dbl, y=skill_level_know), alpha=0.3, color="red") +
  geom_point(aes(x=event_time_dbl, y=skill_level_priority), alpha=0.3, color="yellow") +
  geom_point(aes(x=event_time_dbl, y=skill_level_people), alpha=0.3, color="green") +
  geom_point(aes(x=event_time_dbl, y=skill_level_refusal), alpha=0.3, color="blue") +
  geom_point(aes(x=event_time_dbl, y=skill_level_me), alpha=0.3 ) +
  ylab("Skill Level") +
  xlab("Event Time Max") +
  facet_wrap(~gender)

