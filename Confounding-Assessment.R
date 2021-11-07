library(dslabs)
library(dplyr)
data("research_funding_rates")
research_funding_rates

tab <- tibble(Gendre = c("Male", "Female"), Awarded = c(sum(research_funding_rates$awards_men), sum(research_funding_rates$awards_women)), NotAwarded = c(sum(research_funding_rates$applications_men) - sum(research_funding_rates$awards_men), sum(research_funding_rates$applications_women) - sum(research_funding_rates$awards_women)))

cat("\014")
colnames(tab) <- c("Gender", "Awarded", "Not_Awarded")
tab
cat("\014")
map <- tab$Awarded[1]/(tab$Awarded[1]+tab$Not_Awarded[1])*100
wap <- tab$Awarded[2]/(tab$Awarded[2]+tab$Not_Awarded[2])*100
map
wap

cat("\014")
tab %>% select(-Gender) %>% summarise(tidy(chisq.test(.)))