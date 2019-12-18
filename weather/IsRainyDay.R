rm(list = ls())


library(dplyr)

setwd("~/GitRepo/Multicampus_semi/weather/")
files = dir(pattern = "*.csv")

res = NULL

for (file in files){
  year_data = read.csv(file, fileEncoding = "euc-kr")
  year_data = year_data[, -1]
  
  names(year_data) = c("date", "Temp", "Precipi", "Wind", "Snow")
  year_data$date_ = as.Date(year_data$date, format = "%Y-%m-%d %H:%M")
  year_data$date = paste0(substr(year_data$date_, 1, 4), 
                          substr(year_data$date_, 6, 7),
                          substr(year_data$date_, 9, 10))
  
  proced = year_data %>% filter(Precipi >= 5) %>% select(date)
  final = unique(proced)
  final$IsRainy = 1
  
  res = rbind(res, final)
}

res$date = unique(res$date)
View(res)
