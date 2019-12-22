rm(list = ls())

path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load weather data
weather_data = c("weather_2017.csv", "weather_2018.csv", "weather_2019.csv")

weather_res = NULL

for (file in weather_data){
  year_data = read.csv(file, fileEncoding = "euc-kr", stringsAsFactors = F)
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
              maen_snow = mean(snow, na.rm = T)) %>% 
    mutate(IsRainyDay = ifelse(mean_precipi >= 1, 1, 0))

  weather_res = rbind(weather_res, proced)
} 

View(weather_res)

# load airquality data
air_data = c("air_quality_2017.csv", "air_quality_2018.csv", "air_quality_2019.csv")
