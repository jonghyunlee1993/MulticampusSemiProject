rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/data_proced/")

depart_data = read.csv("final_department.csv")
weather_dust_data = read.csv("weather_dust_weekend_final.csv")
visit_pop_data = read.csv("~/GitRepo/Multicampus_semi/local_people/visit_pop_year_by.csv")

depart_data[1,]

visit_pop = visit_pop_data[, c(2,3,7)]
wd = weather_dust_data[, c(3, 20, 21)]
wd$IsRainy[is.na(wd$IsRainy)] = 0

library(dplyr)
df = inner_join(visit_pop, wd, by = 'date')
