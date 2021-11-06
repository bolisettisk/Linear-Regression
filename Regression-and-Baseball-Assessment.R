library(dplyr)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(Lahman)
library(HistData)
library(broom)

cat("\014")

fit <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB, 
         HR = HR,
         R = R) %>%  
  lm(R ~ BB + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

cat("\014")

get_slope <- function(data){
  fit <- tidy(lm(R ~ BB + HR, data = data), conf.int = TRUE)
  data.frame(BB = fit$estimate[2], conf_low = fit$conf.low[2], conf_high = fit$conf.high[2])
}
  
# Teams %>%
#   filter(yearID %in% 1961:2018) %>% 
#   group_by(yearID) %>%
#   do(get_slope(.)) %>% pivot_longer(-yearID, names_to = "Variable", values_to = "Slope") %>% ggplot(aes(yearID, Slope, color = Variable)) + geom_point()

cat("\014")
Teams %>%
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(get_slope(.)) %>% ggplot(aes(yearID, BB, ymin = conf_low, ymax = conf_high)) + geom_point() +  geom_smooth(method = loess)

cat("\014")
nm <- Teams %>%
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(get_slope(.))
cat("\014")
summary(lm(BB ~ yearID, data = nm))

# cat("\014")
# Teams %>%
#   filter(yearID %in% 1961:2018) %>% summarise(tidy(lm(R ~ BB + HR), conf.int = TRUE)) %>% filter(term == "BB")
# Teams %>%
#   filter(yearID %in% 1961:2018) %>% 
#   group_by(yearID) %>%
#   do(get_slope(.))