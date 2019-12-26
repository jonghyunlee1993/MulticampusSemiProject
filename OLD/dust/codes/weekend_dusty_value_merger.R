rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/dust/")
files = c("dust_grade_weekend_2017.csv", "dust_grade_weekend_2018.csv",
          "dust_grade_weekend_2019.csv")

res = NULL
for (file in files){ 
  data = read.csv(file)
  res = rbind(res, data)
}
res = res[1:283, ]
res = res %>% select(date, IsDustyDay) %>% arrange(date) %>% 
  filter(date != 20191201)

sum(res$IsDustyDay)
write.csv(res, "IsDustyDay_merged.csv")
