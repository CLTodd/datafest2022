library(ggplot2)
df <- logsRaw %>%
  select(player_id, event_time_dbl, skill_level_know, skill_level_priority,
         skill_level_people, skill_level_refusal, skill_level_me,
         avatar_gender) %>%
  filter(event_time_dbl < 50.00) %>%
  filter(!is.na(avatar_gender))
# 
# na.omit(df$skill_level_know)
# na.omit(df$skill_level_priority)
# na.omit(df$skill_level_people)
# na.omit(df$skill_level_refusal)
# na.omit(df$skill_level_me)
# na.omit(df$avatar_gender)

library(data.table)
library(recipes)
library(dplyr)



# max(df$event_time_dbl < 50.00)
# df$event_time_dbl <- (df$event_time_dbl[1:50])

library(esquisse)
esquisser()

library(ggplot2)

ggplot(df) +
 aes(x = player_id, y = event_time_dbl, fill = avatar_gender, group = avatar_gender) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()


