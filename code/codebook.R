#
# Candace
# 3/26/22
#

library(data.table)

# manually copied from  keys sheet in IVY Game data logged events.xlsx

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

#setwd("./data")
saveRDS(ageKey, "ageKey")
saveRDS(senseKey, "senseKey")
saveRDS(minigameKey, "minigameKey")
saveRDS(genderKey, "genderKey")
saveRDS(raceKey, "raceKey")

# use the following line to read these into other scripts from the main repo directory
# df <- readRDS("./data/ageKey")
