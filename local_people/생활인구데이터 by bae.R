library(dplyr)
library(ggplot2)
setwd("C:\\semi-project\\data\\생활인구 데이터\\LOCAL_PEOPLE_201701")
getwd()

#csv파일 name부분 깨져서 새롭게 입력한 후에 다시 불러온다.
y17m01 <- read.csv("20170101.csv", encoding = "euc-kr", header = T)
str(y17m01)
View(y17m01)


#집계구 수치변환
options(scipen = 100)



#백화점 집계구 불러오기
depart_total <- read.csv("C:\\semi-project\\data\\코딩결과_윤성\\5.백화점_집계구_실제집계구.csv")
head(depart_total)

dp_r_area<-depart_total$실제.집계구
View(dp_r_area)


#1 백화점 해당 집계구 필터링
temp <- y17m01[y17m01$집계구코드 %in% dp_r_area,]
View(temp)


#1-2 백화점 해당 집계구 필터링
ttemp = y17m01 %>%
  filter(집계구코드 %in% dp_r_area) 
View(ttemp)



# 2017년 1월 1일 특성별 추출
#남자
y170101_mn <- temp %>%
  select("시간대구분","집계구코드",contains("남자")) %>% 
  filter(시간대구분 %in%(10:20))

View(y170101_mn)
result_mn <- ggplot(y170101_mn)+
  geom_bar(aes(y170101_fm$시간대구분,y170101_fm$시간대구분),stat="identity")

#여자
y170101_fm <- temp %>%
  select("시간대구분","집계구코드",contains("여자")) %>% 
  filter(시간대구분 %in%(10:20))

View(y170101_fm) 



# 2017년 1월 1일 현대_신촌점 추출

hd_shin<-y170101_mn %>% 
  filter(집계구코드 ==1113075030009)
View(hd_shin)

gp_hd_shin <- ggplot(hd_shin) +
  geom_bar(aes(hd_shin$시간대구분,hd_shin$시간대구분),stat="identity")


# 2017년 1월 1일  롯데 잠실
lt_jam<-y170101_mn %>% 
  filter(집계구코드 ==1124080020103)
View(lt_jam)

gp_lt_jam <- ggplot(lt_jam) +           #백터로
  geom_bar(aes(lt_jam$시간대구분,lt_jam[[6]]),stat="identity")

gp_lt_jam
