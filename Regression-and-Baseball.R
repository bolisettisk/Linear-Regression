library(dplyr)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(Lahman)
library(HistData)
library(broom)

cat("\014")

fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

players <- Batting %>% filter(yearID %in% 1997:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game, BB = sum(BB)/G, singles = sum(H-X2B-X3B-HR)/G, doubles = sum(X2B)/G, triples = sum(X3B)/G, HR = sum(HR)/G, AVG = sum(H)/sum(AB), PA = sum(PA)) %>% filter(PA >= 300) %>% select(-G) %>% mutate(R_hat = predict(fit, newdata = .))

qplot(R_hat, data = players, binwidth = 0.5, color = I("black"))

players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

cat("\014")
