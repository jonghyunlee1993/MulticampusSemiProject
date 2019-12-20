rm(list = ls())

options(scipen = 100)

setwd("~/GitRepo/Multicampus_semi/data_proced/")

depart_data = read.csv("final_department.csv")
weather_dust_data = read.csv("weather_dust_weekend_final.csv")
# visit_pop_data = read.csv("visit_pop_age_by.csv")
load("~/GitRepo/Multicampus_semi/local_people/weekend.Rdata")

depart_data[,1]

# visit_pop = visit_pop_data[, c(2,3,7)]
wd = weather_dust_data[, c(3, 20, 21)]
wd$date = as.character(wd$date)
wd$IsRainy[is.na(wd$IsRainy)] = 0

library(dplyr)

my_visit_data = read.csv("visit_pop_year_by.csv")[, c(2,3,4)]
df2 = my_visit_data
df2$date = as.character(df2$date)
df2 = inner_join(df2, wd, by = "date")
df2$IsDustyDay = as.factor(df2$IsDustyDay)
df2$IsRainy = as.factor(df2$IsRainy)

for (depart in depart_data[, 2]){
  try({temp = df2[df2$집계구 == depart, ]
  
  clean = temp[temp$IsRainy == 0, ]$mean_visit_pop
  dusty = temp[temp$IsRainy == 1, ]$mean_visit_pop
  
  # res = t.test(dusty, clean, alternative = "two.sided")
  res = t.test(dusty, clean, alternative = "less")
  
  print(depart_data[depart_data$code == depart, ]$name)
  print(res)})
}
