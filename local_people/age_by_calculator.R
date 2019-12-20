rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/local_people/")
load("weekday.Rdata")
weekday = res
weekday[weekday == "" | weekday == "*"] = 0 

weekday[[1]] = as.character(weekday[[1]])

for (idx in 5:length(weekday)){
  weekday[[idx]] = as.numeric(weekday[[idx]])
  # weekday[is.na(weekday[[idx]]), ][[idx]] = 0
}

load("weekend.Rdata")
weekend = res
weekend[weekend == "" | weekend == "*"] = 0 

weekend[[1]] = as.character(weekend[[1]])

for (idx in 5:length(weekend)){
  weekend[[idx]] = as.numeric(weekend[[idx]])
  # weekend[is.na(weekend[[idx]]), ][[idx]] = 0
}

setwd("~/GitRepo/Multicampus_semi/data_proced/")
depart_list = read.csv("final_department.csv")

## 
library(dplyr)
get_resid_pop_year = function(){
  res = weekday %>% mutate(year = substr(date, 1,4)) %>% 
    group_by(year, 집계구) %>% summarise(mean_m20 = mean(남20, na.rm = T, trim = 10),
                                      mean_m25 = mean(남25, na.rm = T, tirm = 10),
                                      mean_m30 = mean(남30, na.rm = T, tirm = 10),
                                      mean_m35 = mean(남35, na.rm = T, tirm = 10),
                                      mean_m40 = mean(남40, na.rm = T, tirm = 10),
                                      mean_m45 = mean(남45, na.rm = T, tirm = 10),
                                      mean_m50 = mean(남50, na.rm = T, tirm = 10),
                                      mean_m55 = mean(남55, na.rm = T, tirm = 10),
                                      mean_f20 = mean(여20, na.rm = T, trim = 10),
                                      mean_f25 = mean(여25, na.rm = T, trim = 10),
                                      mean_f30 = mean(여30, na.rm = T, trim = 10),
                                      mean_f35 = mean(여35, na.rm = T, trim = 10),
                                      mean_f40 = mean(여40, na.rm = T, trim = 10),
                                      mean_f45 = mean(여45, na.rm = T, trim = 10),
                                      mean_f50 = mean(여50, na.rm = T, trim = 10),
                                      mean_f55 = mean(여55, na.rm = T, trim = 10)) %>% 
    select(year, 집계구, mean_m20, mean_m25, mean_m30, mean_m35, mean_m40, mean_m45, mean_m50, mean_m55,
           mean_f20, mean_f25, mean_f30, mean_f35, mean_f40, mean_f45, mean_f50, mean_f55)
  
  
  return(res)
}

resid_pop = get_resid_pop_year()
# write.csv(resid_pop, "resid_pop_year_by.csv")

get_visit_pop_day = function(){
  res = weekend %>% mutate(year = substr(date, 1,4)) %>% 
    group_by(date, 집계구) %>% summarise(mean_m20 = mean(남20, na.rm = T, trim = 10),
                                      mean_m25 = mean(남25, na.rm = T, tirm = 10),
                                      mean_m30 = mean(남30, na.rm = T, tirm = 10),
                                      mean_m35 = mean(남35, na.rm = T, tirm = 10),
                                      mean_m40 = mean(남40, na.rm = T, tirm = 10),
                                      mean_m45 = mean(남45, na.rm = T, tirm = 10),
                                      mean_m50 = mean(남50, na.rm = T, tirm = 10),
                                      mean_m55 = mean(남55, na.rm = T, tirm = 10),
                                      mean_f20 = mean(여20, na.rm = T, trim = 10),
                                      mean_f25 = mean(여25, na.rm = T, trim = 10),
                                      mean_f30 = mean(여30, na.rm = T, trim = 10),
                                      mean_f35 = mean(여35, na.rm = T, trim = 10),
                                      mean_f40 = mean(여40, na.rm = T, trim = 10),
                                      mean_f45 = mean(여45, na.rm = T, trim = 10),
                                      mean_f50 = mean(여50, na.rm = T, trim = 10),
                                      mean_f55 = mean(여55, na.rm = T, trim = 10)) %>% 
    select(date, 집계구, mean_m20, mean_m25, mean_m30, mean_m35, mean_m40, mean_m45, mean_m50, mean_m55,
           mean_f20, mean_f25, mean_f30, mean_f35, mean_f40, mean_f45, mean_f50, mean_f55)
  
  return(res)
}

visit_pop = get_visit_pop_day()
visit_pop = visit_pop %>% mutate(year = substr(date, 1, 4))

visit_pop = left_join(visit_pop, resid_pop, by=c("year", "집계구"))
# visit_pop$cleaned_pop = visit_pop$mean_visit_pop - visit_pop$mean_resid_pop

final = NULL
final$date = visit_pop$date
final$dist = visit_pop$집계구

final$m20 = visit_pop$mean_m20.x - visit_pop$mean_m20.y
final$m25 = visit_pop$mean_m25.x - visit_pop$mean_m25.y
final$m30 = visit_pop$mean_m30.x - visit_pop$mean_m30.y
final$m35 = visit_pop$mean_m35.x - visit_pop$mean_m35.y
final$m40 = visit_pop$mean_m40.x - visit_pop$mean_m40.y
final$m45 = visit_pop$mean_m45.x - visit_pop$mean_m45.y
final$m50 = visit_pop$mean_m50.x - visit_pop$mean_m50.y
final$m55 = visit_pop$mean_m55.x - visit_pop$mean_m55.y

final$f20 = visit_pop$mean_f20.x - visit_pop$mean_f20.y
final$f25 = visit_pop$mean_f25.x - visit_pop$mean_f25.y
final$f30 = visit_pop$mean_f30.x - visit_pop$mean_f30.y
final$f35 = visit_pop$mean_f35.x - visit_pop$mean_f35.y
final$f40 = visit_pop$mean_f40.x - visit_pop$mean_f40.y
final$f45 = visit_pop$mean_f45.x - visit_pop$mean_f45.y
final$f50 = visit_pop$mean_f50.x - visit_pop$mean_f50.y
final$f55 = visit_pop$mean_f55.x - visit_pop$mean_f55.y

final_df = as.data.frame(final)
write.csv(final_df, "visit_pop_age_by.csv")
