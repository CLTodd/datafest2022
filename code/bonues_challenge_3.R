# this is not correct, it is partial

library(ggplot2)

df <- select(logs, player_id, skill_level_know, skill_level_priority, 
             skill_level_people, skill_level_refusal, skill_level_me, 
             event_time_dbl, avatar_gender)

df2 <- df %>%
  group_by(player_id) %>%
  filter(event_time_dbl < 50000) %>%
df2$Max <- pmax(df2$skill_level_know, df2$skill_level_priority, df2$skill_level_people, 
                df2$skill_level_refusal, df2$skill_level_me)

gender <- df %>%
  filter(avatar_gender != 'NA')

df3 <- df2 %>%
  filter(Max != 'NA') %>%
  group_by(player_id) %>%
  slice_tail()

combined <- merge(gender, df3, by="player_id") %>%
  select(player_id, avatar_gender.x, skill_level_know.y, skill_level_priority.y, skill_level_people.y, 
         skill_level_refusal.y, skill_level_me.y, event_time_dbl.y, Max)

combinedF <- filter(combined, avatar_gender.x == 'Female')
combinedM <- filter(combined, avatar_gender.x == 'Male')

ggplot(data=combinedF) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_know.y, color="know")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_priority.y, color="priority")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_people.y, color="people")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_refusal.y, color="refusal")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_me.y, color="me")) +
  labs(x="Event Time for Females", y="Skill Level", title="Skill Level by Event Time for Females")

ggplot(data=combinedM) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_know.y, color="know")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_priority.y, color="priority")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_people.y, color="people")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_refusal.y, color="refusal")) + 
  geom_point(aes(x=event_time_dbl.y, y=skill_level_me.y, color="me")) + 
  labs(x="Event Time for Males", y="Skill Level", title="Skill Level by Event Time for Males")
