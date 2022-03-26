# bonus challenge 2
df <- logs %>%
  select(player_id, event_time_dbl, skill_level_know) %>%
  group_by(player_id, skill_level_know) %>%
  slice(1)

model <- lm(skill_level_know ~ event_time_dbl, data=df)
summary(model)

