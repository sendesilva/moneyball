### Confounding: correlation is not causation
library(tidyverse)
library(tidyr)
library(broom)
library(dslabs)
ds_theme_set()
## Spurious correlation
# Using Monte Carlo to demonstrate data dredging / spurious correlation
N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

# dredge for max correlations between X and Y which are generated as random independant 
# normally distributed data
res <- sim_data %>%
  group_by(group) %>%
  summarise(r = cor(X, Y)) %>%
  arrange(desc(r))
res

# plotting shows a convincing correlation between X and Y
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(X,Y)) +
  geom_point() +
  geom_smooth(method = "lm")

# P-values of max correlations are very low
sim_data %>%
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))

## Outliers
set.seed(1)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])
tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)
cor(x,y) # [1] 0.9881391
# correlation without outlier is much lower - 
# hence one solution is to ID and remove outliers
cor(x[-23], y[-23]) # [1] -0.001066464

# alt method is to use Spearman correlation which correlates the ranks of the values
# instead of the values
tibble(x,y) %>% 
  ggplot(aes(rank(x),rank(y))) + 
  geom_point(alpha = 0.5)

cor(rank(x), rank(y)) # [1] 0.06583858
cor(x, y, method = "spearman") # [1] 0.06583858

## Reversal of cause and effect
# using Galton data - investigate father's height as a function of son's
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>%
  do(tidy(lm(father ~ son, data = .)))

## Confounders
# Admission data from six U.C. Berkeley majors, from 1973, showed that more men were 
# being admitted than women: 44% men were admitted compared to 30% women. 
data(admissions)
admissions
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# A statistical test clearly rejects the hypothesis that gender and admission are 
# independent:
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted/100*applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# closer inspection of data by major:
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus = women - men)

# plot the total percent admitted to a major versus the percent of women 
# that make up the applicants.
admissions %>%
  group_by(major) %>%
  summarise(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender=="women")/sum(applicants))*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# applicants for each major by gender
admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# acceptance for each major by gender
admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

# acceptance difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))


## Simpson's Paradox
# 3 variables x,y which are negatively correlated and z
N <- 100
Sigma <- matrix(c(1,0.75,0.75, 1), 2, 2)*1.5
means <- list(c(11,3), c(9,5), c(7,7), c(5,9), c(3,11))
dat <- lapply(means, function(mu) 
  MASS::mvrnorm(N, mu, Sigma))
dat <- tbl_df(Reduce(rbind, dat)) %>% 
  mutate(Z = as.character(rep(seq_along(means), each = N)))
names(dat) <- c("X", "Y", "Z")
dat %>% ggplot(aes(X,Y)) + geom_point(alpha = .5) +
  ggtitle(paste("correlation = ", round(cor(dat$X, dat$Y), 2)))

# stratifying x and y by z shows positive correlation between x and y  
means <- tbl_df(Reduce(rbind, means)) %>% setNames(c("x","y")) %>%
  mutate(z = as.character(seq_along(means)))

corrs <- dat %>% group_by(Z) %>% summarize(cor = cor(X,Y)) %>% .$cor 

dat %>% ggplot(aes(X, Y, color = Z)) + 
  geom_point(show.legend = FALSE, alpha = 0.5) +
  ggtitle(paste("correlations =",  paste(signif(corrs,2), collapse=" "))) +
  annotate("text", x = means$x, y = means$y, label = paste("Z=", means$z), cex = 5)  


