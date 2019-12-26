rm(list = ls())
options("scipen" = 100)

# path 
path_data = "/Volumes/Transcend/Card_subway_dataset/"
path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load 집계구 
department_data = read.csv(paste0(path_meta, "department.csv"))
stations = department_data$subway_code

# load subway dataset
subway_files = dir(path = path_data, pattern = "*.csv")

# make sequential data and parse only weekend 
days = seq.Date(as.Date("2017/1/1"), as.Date("2019/11/30"), by="day")
dow = weekdays(days)
weekend = days[dow %in% c("Saturday", "Sunday")]
weekend_num = as.integer(paste0(substr(as.character(weekend), 1, 4),
                     substr(as.character(weekend), 6, 7),
                     substr(as.character(weekend), 9, 10)))

library(dplyr)

res = NULL
for (file in subway_files){
  cat(file, "\n")
  
  temp = read.csv(file, row.names = NULL)
  names(temp) = c("date", "line", "station_no", "station_name",
                  "departure", "arrival", "a")
  subway_month = temp %>% 
    filter(date %in% weekend_num & station_no %in% stations) %>% 
    select(-c("line", "departure", "a"))
  
  res = rbind(res, subway_month)
}

write.csv(res, paste0(path_res, "weekend_subway.csv"))
  