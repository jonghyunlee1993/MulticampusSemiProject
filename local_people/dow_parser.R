rm(list = ls())
options("scipen" = 100)

# load 집계구 
setwd("~/GitRepo/Multicampus_semi/address/")
loc = read.csv("department_nearest_sector.csv")
# 단순 최단 거리 기준
temp_loc = unlist(loc[1, seq(2,ncol(loc), 2)])


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

# 추후 for loop 로 변경
fname = paste0("LOCAL_PEOPLE_", format(weekend[1], "%Y%m%d"), ".csv")
data = read.csv(fname, fileEncoding = "euc-kr")
parse_data = data[, c(2, 4, 5, 7:18, 21:32)]
names(parse_data) = c(default_names, male_names, female_names)

open_hours = parse_data[parse_data$시간대 %in% c(10:22) & parse_data$집계구 %in% temp_loc, ]

