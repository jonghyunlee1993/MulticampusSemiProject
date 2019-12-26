subway = c("신길", 
"신도림",
"천호",
"삼성",
"압구정",
"신촌",
"오목교",
"미아사거리",
"잠실",
"을지로입구",
"노원",
"한티",
"건대입구",
"신림",
"잠실",
"미아사거리")

setwd("/Volumes/Transcend/Card_subway_dataset/")
data = read.csv("CARD_SUBWAY_MONTH_201702.csv")

library(dplyr)
temp = unique(data[data$역명 %in% subway, c(2:4)])
View(temp)

temp = unique(data[data$역ID, c(2:4)])
temp = temp %>% arrange(temp$역ID)
View(temp)
