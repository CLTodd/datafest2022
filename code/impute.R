library(data.table)
library(dplyr)
s5wide <- readRDS("V:\\Downloads\\wides5")
dem <- readRDS("V:\\Downloads\\logsDemographics")
dem$player_id <- as.character(dem$player_id)
full <- left_join(s5wide, dem, by="player_id")
full$gender <- as.factor(full$gender)
full$genderCode <- NULL
full$race <- as.factor(full$race)
full$raceCode <- NULL
full$school <- as.factor(full$school)

library("FastImputation")

#patterns <-
#  TrainFastImputation(x=full, 
#                      idvars=c(1,9,11),
#                      categorical=c(8,10,12))

fullOnly <- copy(full)
fullOnly[,c(1,9,11)] <- NULL

fullOnly <-
fullOnly %>%
  rename(week0=`0`,week3=`3`,week6=`6`,week12=`12`,week24=`24`)

temp <-
  missRanger(data=fullOnly,
             seed=16802,
             maxiter=5,
             verbose=1)
