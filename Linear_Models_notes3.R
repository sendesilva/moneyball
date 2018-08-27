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


  #lm(R ~ BB + singles + doubles + triples + HR, data = .)
