### Building a better offensive metric for baseball
library(Lahman)
library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyr)
ds_theme_set()

# Using lm for 2 predictive variables: runs/game as a function of walks/G and HR/G
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)
# then using tidy func to view estimates and stats for linear model
library(broom)
tidy(fit, conf.int = TRUE)

# To construct a metric to pick players, in addtion to HR and BB, we need to consider 
# singles, doubles, and triples as well. We're going to take somewhat of a leap of faith and 
# assume that these five variables are jointly normal:
# Yi =  β0 +  β1 xi,1 + β2 xi,2  + β3 x1,3  + β4 xi,4  + β5 xi,5 +  ϵi , i = 1, ..., N 
# ...with x1, x2, x3, x4, x5 representing bases on balls per game, singles per game, 
# doubles per game, triples per game, and home runs per game, respectively.
# Using lm, we can quickly find the least square errors for the parameters:

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

coefs <- tidy(fit, conf.int = TRUE)
coefs

# testing accuracy of model by predicting 2002 for the various teams and comparing
# with actual 2002 data which have not been using in fitting the model
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# To make the per-game team rate comparable to the per-plate-appearance player rate, 
# we compute the average number of team plate appearances per game 
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarise(pa_per_game = sum(AB+BB)/max(G)) %>%
  .$pa_per_game %>%
  mean

# compute/predict the per-plate-appearance rates for players available in 2002 
# based on previous years data and avoid small sample artifacts by filtering out 
# players with few plate interferences, then compare with actual data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

players %>% ggplot(aes(R_hat)) +
  geom_histogram(binwidth = 0.5, color = "black")
# The distribution shows that there's y variability across players, as we can see here.

# To actually build the teams, we will need to know the players' salaries, since we have 
# a limited budget. Salaries are added:
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
players <- Fielding %>% filter(yearID == 2002) %>%
  filter(!POS %in% c("OF","P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# Finally, we add their names and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")

# print table with our predicted run statistic, some other statistic, 
# the player's name, their position, and their salary.
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

# plot metrics against salary
players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

# removing young players who may not have negotiated new salaries
players %>% filter(debut < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

# # 2.5.1 Q3 using regression formula to predict runs
# A <- c(BB = 2, singles = 4, doubles = 1, triples = 0, HR = 1)
# B <- c(BB = 1, singles = 6, doubles = 2, triples = 1, HR = 0)
# BB <- 1
# singles <- 6
# doubles <- 2
# triples <- 1
# HR <- 0
# R = -2.769 + 0.371*BB + 0.519*singles + 0.771*doubles + 1.240*triples + 1.443*HR
# R # if A, R= 2.263; if B, R = 3.498

# recommit
