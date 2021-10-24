library(dplyr)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(Lahman)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2) 

dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata) 


cat("\014")
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))