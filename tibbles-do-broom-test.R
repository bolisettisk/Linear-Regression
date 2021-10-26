library(dplyr)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(Lahman)
library(HistData)
library(broom)


get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}


cat("\014")
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")



data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
# set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

cat("\014")

galton %>% group_by(pair) %>% summarise(length(pair))

galton %>% group_by(pair) %>% summarise(cor(parentHeight, childHeight))


cat("\014")
galton %>% group_by(pair) %>% summarise(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) %>% filter(term == "parentHeight") %>% ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_errorbar() + geom_point()

cat("\014")
galton %>% group_by(pair) %>% summarise(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) %>% filter(term == "parentHeight") 


  