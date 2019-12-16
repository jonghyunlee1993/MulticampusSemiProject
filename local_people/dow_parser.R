rm(list = ls())
options("scipen" = 100)

# load 집계구 
setwd("~/GitRepo/Multicampus_semi/address/")
loc = read.csv("proced_department.csv")

codes = loc$code

# load local people dataset
setwd("/Volumes/MnD_Lee_SSD/Local_people_dataset")
files = dir(pattern = "*.csv")

# parse fisrt and last date
head(files, 1)
tail(files, 1)

# make sequential data and parse only weekend 
days = seq.Date(as.Date("2017/1/1"), as.Date("2019/11/30"), by="day")
dow = weekdays(days)
weekend = days[dow %in% c("Saturday", "Sunday")]
head(weekend)

# rename col index
default_names = c("시간대", "집계구", "총생활인구")
male_names = paste0("남", seq(10, 65, 5))
female_names = paste0("여", seq(10, 65, 5))


res = NULL

for (my_date in 1:length(weekend)){
  cat(my_date, "\n")
  fname = paste0("LOCAL_PEOPLE_", format(weekend[my_date], "%Y%m%d"), ".csv")
  
  try({data = read.csv(fname, fileEncoding = "UTF-8")})

  parse_data = data[, c(2, 4, 5, 7:18, 21:32)]
  names(parse_data) = c(default_names, male_names, female_names)
  
  open_hours = parse_data[parse_data$시간대 %in% c(10:22) & parse_data$집계구 %in% codes, ]
  # View(open_hours)
  # barplot(open_hours[open_hours$집계구 == codes[14], 3])
  date_res = cbind(date = rep(format(weekend[my_date], "%Y%m%d"), nrow(open_hours)), open_hours)
  res = rbind(res, date_res)
}


