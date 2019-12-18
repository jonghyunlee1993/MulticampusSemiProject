rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/SKT_dataset/")

library(reshape2)
library(dplyr)
files = dir(pattern = "*.csv")

# data = read.csv(files[1], fileEncoding = "UTF-8")
# names(data) = c("date", "time", "age", "sex", "si", "gungu", "pop")
# head(data)
# jongro = data %>% filter(gungu == "종로구") %>% select(-c(si))
# temp = dcast(jongro, date + time + gungu ~ age + sex, mean, value.var = "pop")
# View(temp)

parse_weekend = function(dow){
  date_list = as.Date(as.character(dow$date), format = "%Y%m%d")
  dow = weekdays(date_list)
  weekend = dow %in% c("Saturday", "Sunday")
  weekend_list = as.character.Date(date_list[weekend])
  weekend_list = paste0(substr(weekend_list, 1, 4), 
                        substr(weekend_list, 6, 7), 
                        substr(weekend_list, 9, 10))
  
  return(weekend_list)
}


for (file in files){
  data = read.csv(file, fileEncoding = "UTF-8")
  names(data) = c("date", "time", "age", "sex", "si", "gungu", "pop")
  data = data[, -5]
  dow = data %>% select(date) %>% unique() 
  
  weekend_list = parse_weekend(dow)
  
  data_weekend = data %>% filter(date %in% weekend_list)
}