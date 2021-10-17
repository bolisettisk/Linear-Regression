library(Lahman)
library(dplyr)
library(tidyverse)

# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#   ggplot(aes(R_per_game, AB_per_game)) + 
#   geom_point()
# 
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#   ggplot(aes(AB_per_game, R_per_game)) +
#   geom_point(alpha = 0.5)
# 
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#   ggplot(aes(R_per_game, AB_per_game)) + 
#   geom_point(alpha = 0.5)
# 
# 
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(Win_rate = W/G, FE_per_game = E/G) %>%
#   ggplot(aes(FE_per_game, Win_rate)) + 
#   geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G) %>%
  ggplot(aes(triples_per_game, doubles_per_game)) + 
  geom_point(alpha = 0.5)