library(dplyr)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(Lahman)
library(HistData)
library(broom)


set.seed(1989) #if you are using R 3.5 or earlier
#set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

cat("\014")

female_heights %>% summarise(cor(mother, daughter)*sd(mother)/sd(daughter))
cat("\014")
lse <- lm(mother ~ daughter, data = female_heights) %>% .$coef 
lse
lse <- data.frame(b0 = lse[1], b1 = lse[2])
lse
lse$b0 + female_heights$daughter[1]*lse$b1
female_heights$mother[1]

cat("\014")

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) 

cat("\014")


bat_01 <- bat_01 %>% group_by(playerID) %>% summarize(mean_singles = mean(singles), mean_bb = mean(bb))
bat_01 %>% filter(mean_singles > 0.2) %>% pull(mean_singles) %>% length()
bat_01 %>% filter(mean_bb > 0.2) %>% pull(mean_bb) %>% length()

tab <- inner_join(bat_02, bat_01)
head(tab)
tab %>% summarise(cor_single = cor(singles, mean_singles), cor_bb = cor(bb, mean_bb))

tab %>% ggplot(aes(mean_singles, singles)) + geom_point()
tab %>% ggplot(aes(mean_bb, bb)) + geom_point()

cat("\014")
lm(singles ~ mean_singles, tab)
lm(bb ~ mean_bb, tab)












