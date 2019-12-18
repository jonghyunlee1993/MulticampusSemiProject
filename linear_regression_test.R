rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/")
dust_data = read.csv("./dust/IsDustyDay_merged.csv")
visit_data = read.csv("./local_people/visit_pop_year_by.csv")

final_data = left_join(visit_data, dust_data, by = "date")
View(final_data)

View(final_data)
library(dplyr)
test = final_data %>% 
  filter(집계구 == 1102052020001) %>% 
  select(date, cleaned_pop, IsDustyDay)

claen_day = test[test$IsDustyDay == 0, ]$cleaned_pop
dusty_day = test[test$IsDustyDay == 1, ]$cleaned_pop

var.test(claen_day, dusty_day)
t.test(claen_day, dusty_day, paired = F, var.equal = T)


model = lm(cleaned_pop ~ IsDustyDay, data = test)  
summary(model)
