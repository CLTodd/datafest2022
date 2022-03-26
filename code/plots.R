#######################
#### ABE'S SANDBOX ####
#######################

### DATA FEST VIZ 

logs <- read.csv("C:/Users/abrah/Downloads/DataFest-Dataset-2022/For Students-Updated2022-03-24/data files/logs.csv", header=TRUE)
View(logs)

s5_scores <- read.csv("C:/Users/abrah/Downloads/DataFest-Dataset-2022/For Students-Updated2022-03-24/data files/S5_scores_cleaned.csv")
View(s5_scores)

s55 <- s5_scores %>%
  mutate(if_else())


library(tidyr)
library(dplyr)

s5<- select(s5_scores, player_id, S5_mean)
View(s5)

log1 <- select(logs, player_id, school, wave, date)
View(log1)

data <- unique(merge(log1, s5_scores, by = "player_id"))

S5_scores_complete <- read.csv("C:/Users/abrah/Downloads/DataFest-Dataset-2022/For Students-Updated2022-03-24/data files/S5_scores_complete.csv")
View(S5_scores_complete)


library(ggplot2)

ggplot(data = S5_scores_complete, aes(x=as.factor(player_id), y=S5_mean)) +
  geom_boxplot()

### Score through the weeks (with a path)

a <- ggplot(data = S5_scores_complete, aes(x=weeks, y=S5_mean, col=as.factor(player_id))) +  
  geom_path() +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_color_viridis_d() +
  labs(x = "Weeks", y = "Score in assessment")

a

### Score with only points

b <- ggplot(data = S5_scores_complete, aes(x=weeks, y=S5_mean, col=as.factor(player_id))) + 
  geom_point(show.legend = F, alpha = 0.7) +
  scale_color_viridis_d() +
  labs(x = "Weeks", y = "Score in assessment") 
 
b


### Animation RULES
## install gganimate :P
library(gganimate)

b + transition_time(weeks) +
  labs(title = "weeks: {frame_time}") 
