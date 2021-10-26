library(dplyr)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(Lahman)
library(HistData)
library(broom)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 

get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
dat %>%  
  group_by(HR) %>%
  summarize(slope = get_slope(BB, R))

cat("\014")
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>% .$coef

fit <- lm(R ~ BB, dat)

cat("\014")


dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))









