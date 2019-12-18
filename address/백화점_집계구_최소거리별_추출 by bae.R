setwd("C:\\semi-project\\data")
distance <- read.csv("백화점 집계구 거리결과.csv")

department <- read.csv("점포이름_위도_경도_주소.csv")
area <- read.csv("집계코드_위도_경도_주소.csv")

head(distance,5)
head(department,3);nrow(department)
head(area,3)

str(distance)


#value가 최소값인 열index 추출 알고리즘

#1
min(distance[1])
which(distance[2]==min(distance[2]))         

#2-1
#집계구가 칼럼으로 저장됐을 때인 지금
distance$X[which(distance[2]==min(distance[2]))]

#2-2
#원래 목적이었던 이름이었을 경우 
row.names(distance[3]==reuslt)



#for문으로 거리 최소값만의 데이터셋 구하기
#1 : 백화점~집계구 빈 데이터셋
data<-matrix(1:25, ncol=25)
data<- data.frame(data)
names(data) <- names(distance)[-1]
row.name<-"해당 집계구"

data


#1-2 : 백화점~최소거리 빈 데이터셋
min_dist<- c()

#1-3 : 백화점 위도경도 _빈 데이터 셋
de_lat<-c()
de_lon<-c()
de_where <-c()

#1-4 : 집계구 위도 경도 _빈 데이터셋
ar_lon<-c()
ar_lat<-c()
ar_where<-c()


#2 데이터셋 알고리즘
options(scipen = 100)

for (i in (2:length(distance))){
  row_index <- which(distance[i] == min(distance[i]))
  result <- distance$X[row_index]
  #result<-unique(result)   1.117051e+12 1.117051e+12  -> 1117051030401 1117051030402
  
  #집계구역 , 해당 집계구역의 백화점과 거리
  data[1,i-1] <- result[1]
  min_dist[i-1] <- min(distance[i])
  
  #백화점 위도/경도/주소
  de_lon[i-1] <- department[i-1,3]
  de_lat[i-1] <- department[i-1,4]
  de_where[i-1] <-as.character(department[i-1,5][1])
  
  #집계구역 위도/경도/주소
  ar_lon[i-1] <- area[row_index[1],3]  #중복제거, 대표로 맨 앞 추출
  ar_lat[i-1] <- area[row_index[1],4]
  ar_where[i-1] <- as.character(area[row_index,5][1])  
                  #area[row_index,5][1] ~ 팩터 levels값으로 출력된다 _문자형변환.
}

data
min_dist

final_result <-rbind(data,min_dist,de_lon,de_lat,
                     ar_lon,ar_lat,de_where,ar_where)
row.names(final_result) <- c('해당집계구', '백화점과 집계구 거리','백화점 경도',
                             '백화점 위도','집계구 경도','집계구 위도','백화점 주소',
                             '집계구 주소')

write.csv(final_result,'백화점-집계구_종합판(중복제거).csv')




''''''''''''''''위 : 중복 제거 /  밑 : 중복 포함시키기''''''''''''''''



#2-2
data2 <- list()
options(scipen = 100)
for (i in(2:length(distance))){
  index <- which(distance[i] == min(distance[i]))
  result <- distance$X[index]
  #data2 <- append(data2,result)
  data2 <- append(data2,list(result))
}

data2


#2-3
names(data2) <- names(distance)[-1]
data2

data3 = as.data.frame(unlist(data2))

write.csv(data3,"백화점-집계구(중복포함).csv")
