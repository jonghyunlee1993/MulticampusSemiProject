rm(list = ls())
options("scipen" = 100)

# load 집계구 
setwd("~/GitRepo/Multicampus_semi/address/")
loc = read.csv("proced_department.csv")

codes = unique(loc$code)

# load local people dataset
# setwd("/Volumes/MnD_Lee_SSD/Local_people_dataset")
# files = dir(pattern = "*.csv")
setwd("/Volumes/Transcend/Local_people_dataset/")
files = dir(pattern = "*.csv")

# parse fisrt and last date
head(files, 1)
tail(files, 1)

# make sequential data and parse only weekend 
days = seq.Date(as.Date("2017/1/1"), as.Date("2019/11/30"), by="day")
dow = weekdays(days)
weekday = days[dow %in% c("Tuesday", "Wednesday", "Thursday")]
head(weekday)

# rename col index
default_names = c("시간대", "집계구", "총생활인구")
male_names = paste0("남", seq(10, 65, 5))
female_names = paste0("여", seq(10, 65, 5))

setwd("/Volumes/Transcend/Local_people_dataset/")
res = NULL

residence_hour = function(fname, res){
  data = read.csv(fname, fileEncoding = "UTF-8")
  
  parse_data = data[, c(2, 4, 5, 7:18, 21:32)]
  names(parse_data) = c(default_names, male_names, female_names)
  
  stay_hours = parse_data[parse_data$시간대 %in% c(2:5) & parse_data$집계구 %in% codes, ]
  
  date_res = cbind(date = rep(format(weekday[my_date], "%Y%m%d"), nrow(stay_hours)), stay_hours)
  res = rbind(res, date_res)
  
  return(res)
}

for (my_date in 1:length(weekday)){
  cat(my_date, "/", length(weekday), "\n")
  fname = paste0("LOCAL_PEOPLE_", format(weekday[my_date], "%Y%m%d"), ".csv")
  
  try({res = residence_hour(fname, res)})
}

save(res, file = "weekday.Rdata")
# save(res, file = "~/GitRepo/Multicampus_semi/local_people/weekday.Rdata")
