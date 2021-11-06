library(tidyverse)
library(broom)
library(Lahman)


Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

cat("\014")

TS <- Teams_small %>% mutate(RG = R/G, HRG = HR/G)

TS %>% lm(avg_attendance ~ RG, data = .)
TS %>% lm(avg_attendance ~ HRG, data = .)

cat("\014")
TS %>% lm(avg_attendance ~ W, data = .)

cat("\014")
TS %>% lm(avg_attendance ~ yearID, data = .)

cat("\014")
cor(TS$RG, TS$W)
cor(TS$HRG, TS$W)

cat("\014")
TS %>% mutate(W_Strata = round(W/10)) %>% filter(W_Strata %in% c(5:10)) %>% group_by(W_Strata) %>% filter(W_Strata == 8) %>% .$W_Strata %>% length()
TS

cat("\014")
TS %>% mutate(W_Strata = round(W/10)) %>% filter(W_Strata %in% c(5:10)) %>% group_by(W_Strata) %>% summarise(tidy(lm(avg_attendance ~ RG), conf.int = TRUE)) %>% filter(term == "RG")

TS %>% mutate(W_Strata = round(W/10)) %>% filter(W_Strata %in% c(5:10)) %>% group_by(W_Strata) %>% summarise(tidy(lm(avg_attendance ~ HRG), conf.int = TRUE)) %>% filter(term == "HRG")

cat("\014")
TS %>% lm(avg_attendance ~ RG+HRG+W+yearID, data = .)

cat("\014")
fit <- TS %>% lm(avg_attendance ~ RG+HRG+W+yearID, data = .)
coefs <- tidy(fit, conf.int = TRUE)

Parameters_Data <- data.frame(W = 80, RG = 5, HRG = 1.2, yearID = 2002)
predict(fit, Parameters_Data)


Parameters_Data <- data.frame(W = 80, RG = 5, HRG = 1.2, yearID = 1960)
predict(fit, Parameters_Data)


cat("\014")
fit <- TS %>% lm(avg_attendance ~ RG+HRG+W+yearID, data = .)

Teams %>% filter(yearID %in% 2002) %>% mutate(avg_attendance = attendance/G, RG = R/G, HRG = HR/G) %>% mutate(R_hat = predict(fit, newdata = .)) %>% summarise(cor(avg_attendance, R_hat))




