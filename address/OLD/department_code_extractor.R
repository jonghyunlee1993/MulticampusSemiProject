rm(list = ls())

options(scipen=100)
setwd("~/GitRepo/Multicampus_semi/")

# setup district
district = read.csv("백화점_집계구_실제집계구.csv", fileEncoding = "euc-kr")
district = district[c(1:2, 5:6)]
names(district) = c("name", "code", "lon", "lat")
View(district)

district = district[-c(14, 19, 22, 24, 25), ]
write.csv(district, "proced_department.csv")

