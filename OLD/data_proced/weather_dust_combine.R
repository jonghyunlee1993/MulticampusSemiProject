rm(list = ls())

library(dplyr)

## weather info
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
  
  mean_value = year_data %>% group_by(date) %>% 
    summarise(mean_Temp = mean(Temp, na.rm = T), 
              mean_Precipi = mean(Precipi, na.rm = T),
              mean_Wind = mean(Wind, na.rm = T),
              mean_Snow = mean(Snow, na.rm = T)) %>% 
    select(date, mean_Temp, mean_Precipi, mean_Wind, mean_Snow)

  median_value = year_data %>% group_by(date) %>% 
    summarise(median_Temp = median(Temp, na.rm = T), 
              median_Precipi = median(Precipi, na.rm = T),
              median_Wind = median(Wind, na.rm = T),
              median_Snow = median(Snow, na.rm = T)) %>% 
    select(date, median_Temp, median_Precipi, median_Wind, median_Snow)
  
  max_value = year_data %>% group_by(date) %>% 
    summarise(max_Temp = max(Temp),
              max_Precipi = max(Precipi),
              max_Wind = max(Wind),
              max_Snow = max(Snow)) %>% 
    select(date, max_Temp, max_Precipi, max_Wind, max_Snow)
  
  merger = inner_join(mean_value, median_value, by = "date")
  merger = inner_join(merger, max_value, by = "date")
  
  res = rbind(res, merger)
}

View(res)
write.csv(res, "merged_weather.csv")

weather_df = res
weather_df$date = as.integer(weather_df$date)
rm(merger)

## dust 
setwd("~/GitRepo/Multicampus_semi/dust/data")
files = dir(pattern = "*.csv")

dust_grade = function(dust_avg){
  if (dust_avg >= 151) dust_grade = 8
  else if (dust_avg >= 101) dust_grade = 7
  else if (dust_avg >= 76) dust_grade = 6
  else if (dust_avg >= 51) dust_grade = 5
  else if (dust_avg >= 41) dust_grade = 4
  else if (dust_avg >= 31) dust_grade = 3
  else if (dust_avg >= 16) dust_grade = 2
  else dust_grade = 1
  
  return(dust_grade)
}

fine_grade = function(dust_avg){
  if (dust_avg >= 76) dust_grade = 8
  else if (dust_avg >= 51) dust_grade = 7
  else if (dust_avg >= 38) dust_grade = 6
  else if (dust_avg >= 26) dust_grade = 5
  else if (dust_avg >= 21) dust_grade = 4
  else if (dust_avg >= 16) dust_grade = 3
  else if (dust_avg >= 9) dust_grade = 2
  else dust_grade = 1
  
  return(dust_grade)
}

res = NULL

for (file in files){
  year_data = read.csv(file)[, c(1,2,7,8)]
  names(year_data) = c("date", "loc", "dust", "fine_dust")
  
  year_data = year_data %>% select(-2) %>% group_by(date) %>%  
    mutate(dust_avg = mean(dust, na.rm = T, trim = 10), fine_avg = mean(fine_dust, na.rm = T, trim = 10)) %>% 
    select(date, dust_avg, fine_avg) %>% unique()
  
  year_data$dust_grade = sapply(year_data$dust_avg, dust_grade)
  year_data$fine_grade = sapply(year_data$fine_avg, fine_grade)
  
  year_data$IsDustyDay = ifelse(year_data$dust_grade > 4 | year_data$fine_grade > 4, 1, 0)
  
  res = rbind(res, year_data)
}

View(res)
setwd("/Users/jonghyunlee/GitRepo/Multicampus_semi/dust")
write.csv(res, "dust_merged.csv")

dust_df = res
rm(res)

final_df = inner_join(weather_df, dust_df, by = "date")
write.csv(final_df, "weather_dust_merged.csv")

temp = as.Date(as.character(final_df$date), format = "%Y%m%d")
weekend_df = final_df[weekdays(temp) %in% c("Saturday", "Sunday"), ]
write.csv(weekend_df, "whether_dust_merged_weekend.csv")


