library(tidyverse)
library(HistData)
library(Lahman)

cat("\014")
Teams %>% filter(yearID %in% 1961:2001) %>% mutate(RpG = R/G, BpG = AB/G) %>% summarise(r = cor(RpG, BpG)) %>% pull(r)


Teams %>% filter(yearID %in% 1961:2001) %>% mutate(WpG = W/G, EpG = E/G) %>% summarise(r = cor(WpG, EpG)) %>% pull(r)

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(X2BpG = X2B/G, X3BpG = X3B/G) %>% summarise(r = cor(X2BpG, X3BpG)) %>% pull(r)