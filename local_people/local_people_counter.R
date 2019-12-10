rm(list=ls())

setwd("C:/Users/student/Desktop/Local_people_dataset")
files = dir(pattern = "*.csv")

View(files)
options("scipen" = 100)

temp = read.csv(files[1], header = T)
View(temp)

loc_code = c()

find_loc_people = function(data, loc_code){
  library(dplyr)
  
  res = data %>% filter(집계구코드 %in% loc_code)
  return(res)
}