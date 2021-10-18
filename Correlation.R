library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>% ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5)

cat("\014")
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

R <- sample_n(galton_heights, 25, replace = TRUE) %>% 
  summarize(r = cor(father, son)) %>% pull(r)

B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% 
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

ggplot(aes(sample=R), data = data.frame(R)) + 
  stat_qq() + 
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))



