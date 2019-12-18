#데이터 불러오기 , 구조 살피기
getwd()
setwd("C:/Users/student/Multicampus_semi/address")

department <- read.csv("점포이름_위도_경도_주소.csv")
area <- read.csv("집계코드_위도_경도_주소.csv")


str(department)
str(area)

names(area)[1] <-"areaIndex"
names(area)[2] <-"whereArea"
head(area)

names(department)[1] <-"departIndex"
names(department)[2] <-"departName"
head(department)



#데이터셋 설정
d<-1:{length(department$departIndex)*length(area$areaIndex)}
length(d)

distanceData <- matrix(1:length(d),ncol = length(department$departName))
distData <-data.frame(distanceData)
names(distData) <- department$departName
row.names(distData) <- area$whereArea

View(head(distData))




# 거리계산 함수 : distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
distm(c(127,37.5),c(127,37.6), fun = distHaversine)

# 편하게 dist(x,y)로 설정
dist <- function(x,y,z,w){
  dd<-distm(c(x,y),c(z,w),fun = distHaversine)
  return(dd)
}


nrow(distData)


# 거리계산한 것 데이터셋에 push 하기
str(distData)
head(department)
head(area)
length(area$lon)

for (i in 1:ncol(distData)){
  print(1)
}

for (dpIndex in 1:ncol(distData)){
  for (arIndex in 1:nrow(distData)){
    distance<-dist(department$lon[dpIndex],department$lat[dpIndex],
                   area$lon[arIndex],area$lat[arIndex])
    distData[arIndex,dpIndex]<-distance
  }
}




