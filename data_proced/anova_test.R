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

gangnam = df[df$집계구 == depart_data$code[1], c(3:5)]
gangnam$IsDustyDay = as.factor(gangnam$IsDustyDay)
gangnam$IsRainy = as.factor(gangnam$IsRainy)

model = aov(cleaned_pop ~ IsDustyDay * IsRainy, data = gangnam)
summary(model)

with(gangnam, interaction.plot(IsDustyDay, IsRainy, cleaned_pop, fun = mean,
                             main = "Interaction Plot"))

TukeyHSD(model, which = "IsDustyDay")
TukeyHSD(model, which = "IsRainy")

for (idx in 1:nrow(depart_data)){
  temp = df[df$집계구 == depart_data$code[idx], c(3:5)]
  temp$IsDustyDay = as.factor(temp$IsDustyDay)
  temp$IsRainy = as.factor(temp$IsRainy)
  
  with(temp, interaction.plot(IsDustyDay, IsRainy, cleaned_pop, fun = mean,
                                 main = "Interaction Plot"))
  
  Sys.sleep(5)
}

for (idx in 1:nrow(depart_data)){
  print(depart_data[idx, 1])
  temp = df[df$집계구 == depart_data$code[idx], c(3:5)]
  boxplot(formula = cleaned_pop ~ IsDustyDay, data = temp)
  Sys.sleep(5)
}

for (idx in 12:nrow(depart_data)){
  temp = df[df$집계구 == depart_data$code[idx], c(3:5)]
  clean = subset(temp, IsDustyDay == 0)
  dusty = subset(temp, IsDustyDay == 1)
  
  print(depart_data[idx, 1])
  res = t.test(dusty, clean)
  print(res)
  
  Sys.sleep(8)
}


visit_pop = visit_pop_data[, c(2,3,7)]
wd2 = weather_dust_data[, c(3, 18, 19)]

df2 = inner_join(visit_pop, wd2, by = 'date')

for (idx in 12:nrow(depart_data)){
  temp = df2[df2$집계구 == depart_data$code[idx], c(3:5)]
  model = lm(cleaned_pop ~ dust_grade + fine_grade, data = temp)
  print(depart_data[idx, 1])
  print(summary(model))
  Sys.sleep(5)
}

visit_pop = visit_pop_data[, c(2,3,7)]
wd3 = weather_dust_data[, c(3, 16, 17)]

df3 = inner_join(visit_pop, wd3, by = 'date')

for (idx in c(1:10, 12:16)){
  temp = df3[df3$집계구 == depart_data$code[idx], c(3:5)]
  model = lm(cleaned_pop ~ dust_avg + fine_avg, data = temp)
  print(depart_data[idx, 1])
  print(summary(model))
  Sys.sleep(5)
}

visit_pop = visit_pop_data[, c(2,3,7)]
wd4 = weather_dust_data[, c(3:5, 16, 17)]
df4 = inner_join(visit_pop, wd4, by = 'date')
df4[is.na(df4$mean_Precipi), 5] = 0

for (idx in c(1:10, 12:16)){
  temp = df4[df4$집계구 == depart_data$code[idx], c(3:7)]
  model = lm(cleaned_pop ~ mean_Temp + mean_Precipi + dust_avg + fine_avg, data = temp)
  print(depart_data[idx, 1])
  print(summary(model))
  Sys.sleep(5)
}



