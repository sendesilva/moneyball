# LInear MOdels
# Calcualating RSS / LSE for height son vs height father from Galton data
library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(HistData) # Galton family data
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# we need to minimize RSS to determin beto0 and beta1
# in this demo we simplify from 3-D to 2-D by keeping beta0 constant at 25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() +
  geom_line(aes(beta1, rss), col=2)

# 2.3.1 Q1 What is the LSE estimate for beta1 if we assume beta0 = 36
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() +
  geom_line(aes(beta1, rss), col=2)
# curve shows rss min when beta1 = 0.50

# lm Function provides the LSE
fit <- lm(son ~ father, data = galton_heights)
fit

# summary function provides more info
summary(fit)

# 2.3.2 Run a linear model in R predicting the number of runs per game based on the 
# number of bases on balls and the number of home runs. Remember to first limit your 
# data to 1961-2001.
library(Lahman)
library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyr)
ds_theme_set()
# data("Teams")
# Base on balls/game vs Home Runs/game 
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, BB_per_game = BB/G, R_per_game = R/G)

# lm Function provides the LSE for baseball stats
fit <- lm(R_per_game ~ HR_per_game + BB_per_game, data = Teams)
fit

## Return to galton_heights
# LSE are random variables - use monte carlo to 1000 samples size 50
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

# visual variability of estimates of coeffs
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1,p2, ncol = 2)

# std errors are hard to calculate but can be obtained from the lm function summary
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>% summary

# compare with std errors from monte carlo sim
lse %>% summarise(se_0 = sd(beta_0), se_1 = sd(beta_1))

# advanced note on LSE - LSE can be strongly correlated
lse %>% summarise(cor(beta_0, beta_1)) # -0.99

# review with standardized father heights:
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef
})
cor(lse[1,], lse[2,]) # -0.18

# depicting confidence intervals using geom_smooth
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# using function predict
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
  ggplot(aes(father, Y_hat)) +
  geom_line()

# predict function can also provide std errors and other info needed to 
# produce confidence intervals
fit <- galton_heights %>% lm(son ~ father, data = .)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat) # [1] "fit"            "se.fit"         "df"             "residual.scale"

# testing 2.3.5 quiz Qs - for properly plotting predictions and CLs for 
# of linear model for son's heights

# galton_heights %>% ggplot(aes(father, son)) +
#   geom_point() +
#   geom_smooth() # does not plot linear model

# galton_heights %>% ggplot(aes(father, son)) +
#   geom_point() +
#   geom_smooth(method = "lm") # good

# model <- lm(son ~ father, data = galton_heights)
# predictions <- predict(model, interval = c("confidence"), level = 0.95)
# data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)
# # change as.tibble to as_tibble
# ggplot(data, aes(x = father, y = fit)) +
#   geom_line(color = "blue", size = 1) + 
#   geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
#   geom_point(data = galton_heights, aes(x = father, y = son)) # good with as_tibble

# model <- lm(son ~ father, data = galton_heights)
# predictions <- predict(model)
# data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
# 
# ggplot(data, aes(x = father, y = fit)) +
#   geom_line(color = "blue", size = 1) + 
#   geom_point(data = galton_heights, aes(x = father, y = son)) # no CLs



