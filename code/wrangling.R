# Candace
#
# 2/26/2022
#
#
#
#
setwd("./data")
logsSample <- readRDS("logsSample")

# Coding the waves
library(ggplot2)

ggplot(logsSample) +
  geom_point(aes(x=date, y=wave, group=wave))