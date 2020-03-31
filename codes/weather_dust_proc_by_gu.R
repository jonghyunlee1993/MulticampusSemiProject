rm(list = ls())

library(dplyr)

path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load department data
departments = read.csv(paste0(path_meta, "department.csv"), stringsAsFactors = F)
measuring_stations = departments$measuring_station

# load weather data
weather_data = c("weather_2017.csv", "weather_2018.csv", "weather_2019.csv")

weather_res = NULL

for (file in weather_data){
  year_data = read.csv(paste0(path_meta, file), fileEncoding = "euc-kr", stringsAsFactors = F)
  year_data = year_data[, -1]
  
  names(year_data) = c("date", "temp", "precipi", "wind", "snow")
  year_data$date_ = as.Date(year_data$date, format = "%Y-%m-%d %H:%M")
  year_data$date = as.integer(paste0(substr(year_data$date_, 1, 4), 
                          substr(year_data$date_, 6, 7),
                          substr(year_data$date_, 9, 10)))
  
  proced = year_data %>% group_by(date) %>% 
    summarise(mean_temp = mean(temp, na.rm = T), 
              mean_precipi = mean(precipi, na.rm = T),
              mean_wind = mean(wind, na.rm = T),
              mean_snow = mean(snow, na.rm = T)) %>% 
    mutate(IsRainyDay = ifelse(mean_precipi >= 1, 1, 0))

  weather_res = rbind(weather_res, proced)
} 

View(weather_res)

# load airquality data
air_data = c("air_quality_2017.csv", "air_quality_2018.csv", "air_quality_2019.csv")

# 미세, 초미세 먼지 값 평균 / 상, 하위 10% 제거 
dust_avg = function(data, num){
  if (num == 7){
    dust_val = data %>% 
      select(c(1,num)) %>% 
      rename(date = 측정일시, dust = 미세먼지.....) %>% 
      group_by(date) %>% 
      summarise(dust_avg = mean(dust, na.rm = T, trim = 10))
  }else if (num == 8){
    dust_val = data %>% 
      select(c(1,num)) %>% 
      rename(date = 측정일시, dust = 초미세먼지.....) %>% 
      group_by(date) %>% 
      summarise(dust_avg = mean(dust, na.rm = T, trim = 10))
  }
  
  return(dust_val)
}

# 미세먼지 등급
dust_grade_calculator = function(dust_avg){
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

# 미세먼지 등급
hyper_dust_grade_calculator = function(dust_avg){
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

IsDusty = function(data){
  IsDustyDay = ifelse(data[[1]] > 4 | data[[2]] >4, 1, 0)
  
  return(IsDustyDay)
}

dust_res = NULL

for (file in air_data){
  year_data = read.csv(paste0(path_meta, file), stringsAsFactors = F)

  fine_avg = dust_avg(year_data, 7)
  hyper_avg = dust_avg(year_data, 8)
  
  seoul_dust_by_gu = year_data %>% 
    filter(측정소명 %in% measuring_stations) %>% 
    select(측정일시, 측정소명, 미세먼지....., 초미세먼지.....) 
  
  dust_na_date = seoul_dust_by_gu[is.na(seoul_dust_by_gu$미세먼지.....), ]$측정일시
  dust_na_idx = rownames(seoul_dust_by_gu[is.na(seoul_dust_by_gu$미세먼지.....), ])
  
  ct = 1
  for (dust_date in dust_na_date){
    seoul_dust_by_gu[as.integer(dust_na_idx[ct]), 3] = fine_avg$dust_avg[fine_avg$date == dust_date]
    ct = ct + 1
  }
  
  hyper_na_date = seoul_dust_by_gu[is.na(seoul_dust_by_gu$초미세먼지.....), ]$측정일시
  hyper_na_idx = rownames(seoul_dust_by_gu[is.na(seoul_dust_by_gu$초미세먼지.....), ])
  
  ct = 1
  for (dust_date in hyper_na_date){
    seoul_dust_by_gu[as.integer(hyper_na_idx[ct]), 4] = hyper_avg$dust_avg[hyper_avg$date == dust_date]
    ct = ct + 1
  }
  
  fine_dust_grade = apply(seoul_dust_by_gu[3], 1, dust_grade_calculator)
  hyper_dust_grade = apply(seoul_dust_by_gu[4], 1, hyper_dust_grade_calculator)
  
  IsDustyDay = apply(cbind(fine_dust_grade, hyper_dust_grade), 1, IsDusty)
  
  seoul_dust_by_gu = cbind(seoul_dust_by_gu, fine_dust_grade, hyper_dust_grade, IsDustyDay)
  
  dust_res = rbind(dust_res, seoul_dust_by_gu)
}





IsDusty = function(data){
  IsDustyDay = ifelse(data[[1]] > 4 | data[[2]] >4, 1, 0)
  
  return(IsDustyDay)
}

dust_res = NULL

for (file in air_data){
  year_data = read.csv(file, stringsAsFactors = F)
  fine_avg = dust_avg(year_data, 7)
  hyper_avg = dust_avg(year_data, 8)
  
  fine_dust_grade = apply(fine_avg[2], 1, dust_grade_calculator)
  hyper_dust_grade = apply(hyper_avg[2], 1, hyper_dust_grade_calculator)
  
  IsDustyDay = apply(cbind(fine_dust_grade, hyper_dust_grade), 1, IsDusty)
  
  year_dust = left_join(fine_avg, hyper_avg, by = "date")
  year_dust = cbind(year_dust, fine_dust_grade, hyper_dust_grade, IsDustyDay)
  
  
  
  dust_res = rbind(dust_res, year_dust)
}

names(dust_res) = c("date", "district","fine_dust", "hyper_dust", 
                    "fine_dust_grade", "hyper_dust_grade", "IsDustyDay")

# weather dust merging
weather_dust_df = left_join(dust_res, weather_res, by = "date")
write.csv(weather_dust_df, paste0(path_res, "weather_dust_proc_by_gu.csv"))


