rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/local_people/")
options(scipen = 100)

load_data = function(fname){
  load(fname)
  return(res)
}

weekday = load_data("weekday.Rdata")
weekend = load_data("weekend.Rdata")

library(dplyr)
get_resid_pop_year = function(){
  res = weekday %>% mutate(year = substr(date, 1,4)) %>% 
    group_by(year, 집계구) %>%  summarize(mean_resid_pop = mean(총생활인구, trim = 10))
  
  return(res)
}

resid_pop = get_resid_pop_year()
# write.csv(resid_pop, "resid_pop_year_by.csv")

get_visit_pop_day = function(){
  res = weekend %>% group_by(date, 집계구) %>% 
    summarize(mean_visit_pop = mean(총생활인구))
  
  return(res)
}

visit_pop = get_visit_pop_day()
visit_pop = visit_pop %>% mutate(year = substr(date, 1, 4))

visit_pop = left_join(visit_pop, resid_pop, by=c("year", "집계구"))
visit_pop$cleaned_pop = visit_pop$mean_visit_pop - visit_pop$mean_resid_pop

# write.csv(visit_pop, "visit_pop_year_by.csv")

depart_list = unique(visit_pop$집계구)
temp = visit_pop[visit_pop$집계구 == depart_list[1], ]
head(temp)

plot(temp$date, temp$mean_visit_pop)
abline(h = mean(temp$mean_visit_pop))
abline(v = c(which(temp$date == 20180106), which(temp$date == 20190105)))

# library(ggplot2)
# par(mfrow = c(2,2))
for (test in 1:length(depart_list)){
  temp = visit_pop[visit_pop$집계구 == depart_list[test], ]
  # fig = ggplot(data = temp, aes(date, mean_visit_pop)) + geom_point()
  plot(temp$date, temp$cleaned_pop)
  title(depart_list[test])
  abline(h = mean(temp$mean_visit_pop))
  abline(v = c(which(temp$date == 20180106), which(temp$date == 20190105)))
  
  # print(fig)
  
  invisible(readline(prompt="Press [enter] to continue"))
}


strange_parts = c("1113075030009", "1115051010002", "1123051010203",
                 "1123058040001", "1123063020012", "1125073030007")

for (strange in strange_parts){
  ttt = weekday %>% mutate(year = substr(date, 1, 4)) %>%  
    group_by(year, 집계구) %>% filter(집계구 == strange)
  
  barplot(ttt$총생활인구)
  title(strange)
  
  invisible(readline(prompt="Press [enter] to continue"))
}





