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

                   # CHANGE THIS ON YOUR MACHINE!
logsRaw <- fread("C:/Users/Candace/Downloads/logs.csv")

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

saveRDS(s5wide, file="wides5")

fwrite(temp, file="players.csv")

########## Random things I guess

# From IVY Game data logged events

ageKey <- data.table("age"= c(11, 12, 13, 14),
                     "index"=c(0,1,2,3))

senseKey <- data.table(senseType = c("know sense",
                                     "priority sense",
                                     "people sense"),
                      keyValue = c(0,1,2))

minigameKey <- data.table(gameTopic = c("know sense", 
                                        "priority sense", 
                                        "people sense",
                                        "refusal power", 
                                        "me power"),
                          keyVale = c(0,1,2,3,4))

genderKey <- data.table(gender=c("male, female"),
                        index = c(0,1))

raceKey <- data.table(race = c("African American",
                               "Caucasian",
                               "Hispanic"),
                      index = c(0,1,2))



