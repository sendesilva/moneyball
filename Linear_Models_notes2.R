library(Lahman)
library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyr)
ds_theme_set()

# first create a data frame for each HR/G strata:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR <= 1.2)

# then create a regression line for each strata / group
dat %>%
  group_by(HR) %>%
  summarise(slope = cor(BB, R)*sd(R)/sd(BB))

# we can use the lm function to get the estimated slopes but it not not part tidyverse
# and will ignote group_by and the results will be incorrect
dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef
  
# investigating tibbles
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

# vs. data frames

# 1. Teams vs as_tibble(Teams) is much more readable
# 2. subsetting a tibble returns a tibble but may not be so in dataframes
class(Teams[,20]) # integer
class(as_tibble(Teams[,20])) # tbl
# to access the original vector we need to use the $ sign
class(as_tibble(Teams)$HR) # integer

# tibbles can have complex entries including lists and functions
tibble(id = c(1,2,3), func = c(mean, median, sd))

# do Function serves as a bridge to tidyverse, fit = column name with output of lm
dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# without col name: leads to error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .)) # Error: Results 1, 2,, ... must be data frames, not lm

# function which outputs what we want in a data frame form
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficients[2,2])
}

# naming the output gives a complex tibble which is not very useful
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))
# Then use do() wihtout naming the output since we are already getting a dataframe
dat %>%
  group_by(HR) %>%
  do(get_slope(.))

# if dataframe being returned has more than 1 row they will be concatenated appropriately
# e.g. with slope and intercept
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficients[,2])
}
dat %>%
  group_by(HR) %>%
  do(get_lse(.))

# # testing 2.4.3b Q2
# get_slope <- function(data) {
#   fit <- lm(R ~ BB, data = data)
#   sum.fit <- summary(fit)
#   
#   data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
#              se = sum.fit$coefficients[2, "Std. Error"],
#              pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
# }
# 
# dat %>% 
#   group_by(HR) %>% 
#   do(get_slope(.))

## broom package extracts info from lm and contains 3 functions - tidy, glance and augment
# tidy() returns estimates and related info as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
# adding CIs (CL = 95% ???)
tidy(fit, conf.int = TRUE)

# since output is a data frame we can use it with do()
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))

# we can also choose which data/columns/rows we want to see using filter and select
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# simplifies visualisation vis ggplot
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# glance() returns model-specific outcomes
glance(fit)

# # testing 2.4.4b Q2
# dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
#   mutate(HR = HR/G,
#          R = R/G) %>%
#   select(lgID, HR, BB, R) # lgID = Leaque level

# dat %>% 
#   group_by(lgID) %>% 
#   do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
#   filter(term == "HR") # good shows estimates and stats for 2 leagues

# dat %>% 
#   group_by(lgID) %>% 
#   do(glance(lm(R ~ HR, data = .))) # not useful - no estimate given

# dat %>% 
#   do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
#   filter(term == "HR") # not useful - 1 overall HR stat not related to league 

# dat %>% 
#   group_by(lgID) %>% 
#   do(mod = lm(R ~ HR, data = .)) # not useful due col naming


