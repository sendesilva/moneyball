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

# evaluate correlation between homeruns, BB and singles
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarise(cor(BB,HR), cor(Singles, HR), cor(BB, Singles))
#  0.4039125       -0.1738404      -0.05605071

# to evaluate of BB is still useful for creating runs we examine the relationship
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_pergame = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

# make a scatter plot for each HR strata
dat %>%
  ggplot(aes(BB_per_game, R_pergame)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

# computing the BB slopes for each home run strata 
dat %>%
  group_by(HR_strata) %>%
  summarise(slope = cor(BB_per_game, R_pergame)*sd(R_pergame)/sd(BB_per_game))


# to evaluate of effect of HR changes when stratified against BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_pergame = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

# make a scatter plot for each BB strata
dat %>%
  ggplot(aes(HR_per_game, R_pergame)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

# computing the BB slopes for each home run strata 
dat %>%
  group_by(BB_strata) %>%
  summarise(slope = cor(HR_per_game, R_pergame)*sd(R_pergame)/sd(HR_per_game))










