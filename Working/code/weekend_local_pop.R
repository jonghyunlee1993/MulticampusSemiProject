rm(list = ls())
options("scipen" = 100)

# path 
path_data = "/Volumes/Transcend/Local_people_dataset/"
path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load 집계구 
department_data = read.csv(paste0(path_meta, "department.csv"), stringsAsFactors = F)

# 알수없는 형변환 오류 발생
codes = c(1119074050002, 1117051030003, 1125073030012,1123058040002, 1123051010201, 1113075030010,
  1115051010004, 1108068010004, 1124080020103, 1102052020001, 1111066030001, 1123063020021,
  1105066011201, 1121052010001, 1124077030001, 1109071040007)

# load local people dataset
local_files = dir(path = path_data, pattern = "*.csv")

# parse fisrt and last date
head(local_files, 1)
tail(local_files, 1)

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

open_hour = function(path_data, fname, res){
  data = read.csv(paste0(path_data, fname), fileEncoding = "UTF-8", stringsAsFactors = F)
  
  parse_data = data[, c(2, 4, 5, 7:18, 21:32)]
  names(parse_data) = c(default_names, male_names, female_names)
  
  open_hours = parse_data[parse_data$시간대 %in% c(10:20) & parse_data$집계구 %in% codes, ]
  date_res = cbind(date = rep(format(weekend[my_date], "%Y%m%d"), nrow(open_hours)), open_hours)
  res = rbind(res, date_res)
  
  return(res)
}

for (my_date in 1:length(weekend)){
  cat(my_date, "/", length(weekend), "\n")
  fname = paste0("LOCAL_PEOPLE_", format(weekend[my_date], "%Y%m%d"), ".csv")
  
  try({res = open_hour(path_data, fname, res)})
}

rownames(res) <- 1:nrow(res)
# save(res, file = paste0(path_res, "local_pop_weekend.Rdata"))
write.csv(res, paste0(path_res, "local_pop_weekend.csv"))
