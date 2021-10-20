library(dplyr)
library(tidyverse)

set.seed(1989) #if you are using R 3.5 or earlier
# set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

cat("\014")
female_heights %>% summarise(mm = mean(mother), msd = sd(mother), dm = mean(daughter), dsd = sd(daughter), cc = cor(mother, daughter)) %>% summarise(slope = cc*dsd/msd, int = dm - (slope*mm), pv = cc*cc*100)

female_heights %>% summarise(mm = mean(mother), msd = sd(mother), dm = mean(daughter), dsd = sd(daughter), cc = cor(mother, daughter)) %>% summarise(dm + (cc*dsd/msd*(60-mm)))