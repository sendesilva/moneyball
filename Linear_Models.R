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

