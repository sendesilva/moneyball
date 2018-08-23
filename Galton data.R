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

# summarising data using normal distribution
galton_heights %>%
  summarise(mean(father), sd(father), mean(son), sd(son))

# plotting son v father
galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# evaluating the correlation bet father and son variables:
galton_heights %>% summarise(cor(father, son))

# LR 1.3.3 Sample Correlation as a Random Variable
# total popn is 179
# For a poorer geneticist using a sample of 25 with R as a random variable
R <- sample_n(galton_hieghts, 25, replace = TRUE) %>%
  summarise(cor(father, son))

# Running a monte-carlo simulation to see the distribution of R
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarise(r=cor(father, son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")
mean(R) # [1] 0.4972976
sd(R) # [1] 0.1495462

# Conditional average for sons of fathers = 72 in tall
conditional_avg <- galton_heights %>% filter(round(father) == 72) %>%
  summarise(avg = mean(son)) %>% .$avg
conditional_avg

# Stratification + boxplots shows the distribution of each group
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# plotting the means of the sons
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# to test connection bet son and father plotstandized heights against each other
r <- galton_heights %>% summarise(r = cor(father, son)) %>% .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son = mean(son)) %>% 
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# calculating the slope and intercept of the regression line
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x # [1] 0.5027904
b <- mu_y - m*mu_x # [1] 35.71249

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# plotting data in std units to get intercept 0 and slope rho
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)


# stratify son height by father standardisec height and check if 
# assumption of conditional bivariate distribution holds (bivariate normal dist)
galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)

# there is a second regression line if want to calculate the fathers height 
# given the sons'
m <- r * s_x/s_y # [1] 0.4986676
b <- mu_x - m*mu_y # [1] 33.96539
