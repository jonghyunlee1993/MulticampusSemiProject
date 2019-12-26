rm(list = ls())

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
df = inner_join(visit_pop, wd, by = 'date')

for (idx in 2:nrow(depart_data)){
  for (group in 3:18){
    temp = df[df$집계구 == depart_data$code[idx], c(group, 20, 21)]
    temp$IsDustyDay = as.factor(temp$IsDustyDay)
    temp$IsRainy = as.factor(temp$IsRainy)
    
    print(paste(depart_data[idx, 1], names(df)[group]))
    # with(temp, interaction.plot(IsDustyDay, IsRainy, temp[[1]], fun = mean,
    #                             main = paste(depart_data[idx, 1], names(df)[group])))
    
    model = lm(temp[[1]] ~ temp$IsDustyDay + temp$IsRainy, data = temp)
    print(summary(model))
    
    Sys.sleep(5)
  }
}

my_visit_data = read.csv("visit_pop_year_by.csv")[, c(2,3,4)]
df2 = my_visit_data
df2$date = as.character(df2$date)
df2 = inner_join(df2, wd, by = "date")
df2$IsDustyDay = as.factor(df2$IsDustyDay)
df2$IsRainy = as.factor(df2$IsRainy)

for (depart in depart_data[, 2]){
  temp = df2[df2$집계구 == depart, ]
  
  model = lm(temp$mean_visit_pop ~ IsDustyDay + IsRainy, data = temp)
  print(depart_data[depart_data$code == depart, ]$name)
  print(summary(model))
}


boxplot(mean_visit_pop ~ IsDustyDay, data = df2)

clean = df2[df2$IsRainy == 0, ]$mean_visit_pop
dusty = df2[df2$IsRainy == 1, ]$mean_visit_pop
t.test(clean, dusty)
