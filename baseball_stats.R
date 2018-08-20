library(Lahman)
library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyr)
ds_theme_set()

# Runs/game vs Home Runs/game 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
# Shows a good correlation with slope given as 1.845

# Stolen Bases/game vs Home Runs/game 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
# Shows poor correlation with calculated slope = 0.278

# Base on balls/game vs Home Runs/game 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
# Shows better correlation with slope = 0.735 (although not as strong as HR vs R)

