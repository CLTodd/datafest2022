######
# Candace
# 3/25/22
#
# This may get changed later, this is basically a sandbox

setwd("./data")
library(data.table)
library(naniar)
library(dplyr)
library(tidyr)

####### log data

logsRaw <- fread("C:/Users/Candace/Downloads/logs.csv")

# Replace "NA" strings with NA values
# it's weird that there's already a mix of NA strings and values in here
# Is that worth worrying about?
logs <- replace_with_na_all(logsRaw, ~.x=="NA")
# when I glanced at the table I only saw "NA" but I added "na" just in case

# Number of schools
logs %>%
  group_by(school) %>%
  summarise(count=n())

# Number of players in the log
players <-
logs %>%
  group_by(player_id) %>%
  summarise(count=n())

# Number of players at each school
players %>%
  group_by(school) %>%
  summarise(count=n())

###### s5 data

s5Raw <- fread("./S5_scores_cleaned.csv")

# Replace "NA" strings with NA values
s5 <- replace_with_na_all(s5Raw, ~.x=="NA")

# number of s5 entries
s5player <-
s5 %>%
  group_by(player_id) %>%
  summarise(count=n())

# s5 wide format
s5wide <- 
  tidyr::pivot_wider(s5Raw, id_cols=player_id, names_from=weeks, values_from =S5_mean)



  
  
# Number of players in the log
players <-
  logs %>%
  group_by(player_id) %>%
  summarise(count=n())

# number of s5 entries
s5player <-
  s5 %>%
  group_by(player_id) %>%
  summarise(count=n())

# players in both
sum(s5player$player_id%in%players$player_id)

# s5 wide with no NA
s5wideNoNa <-
  s5wide %>%
  filter(!is.na(`0`), !is.na(`3`), !is.na(`6`), !is.na(`12`), !is.na(`24`), !is.na(player_id)) 

s5wideNoNa$player_id <- as.numeric(s5wideNoNa$player_id)

# joining
left <- left_join(s5wideNoNa, players, by="player_id")
head(left)
dim(left)


temp <- data.table(left$player_id)

saveRDS(s5wide, file="wides5")

fwrite(temp, file="players.csv")

sum(is.na(s5$S5_mean))
View(s5)


