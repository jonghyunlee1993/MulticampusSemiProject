rm(list=ls())
library(dplyr)
library(stringr)

#데이터 탐색
getwd()
setwd("c:/semi-project/data/생활인구_데이터")
dm <- read.csv("c:/semi-project/data/코딩결과_윤성/5.백화점_집계구_실제집계구.csv")


#수 범위 설정_집계구 
options(scipen = 100)


#알고리즘 : 폴더명, csv파일명 입력하면 읽기
readfile <- function(folder_name,file_name){
  file_data <-read.csv(paste("./",file_name, sep =''), encoding = "UTF-8")
  return(file_data)
}


#데이터셋 설정
weekday_df <- data.frame()
weekend_df <- data.frame()


#파일 수 확인
length(dir(path= 'c:/semi-project/data/생활인구_데이터'))  #35

#데이터셋 설정 알고리즘 
for (i in 1:35 ){
  setwd('c:/semi-project/data/생활인구_데이터')
  folder_name <- dir()[i]
  cat("\n","<<  ", folder_name,"  >>","\n") #진행사항 확인
  
  setwd(paste0('c:/semi-project/data/생활인구_데이터/',folder_name))
  file_list <- length(dir())
  
  #초기화
  mon_weekday_df <- data.frame()
  mon_weekend_df <- data.frame
  
  
  for (z in 1:file_list ){
    file_name <- dir()[z]
    check <- ""
    
    #파일 읽기 : file_data 리턴 , tryCatch로 에러나도 수행 및 확인!
    tryCatch({
      file_data = readfile(folder_name,file_name)
    }, warning=function(w){
      cat("-----------  WARNNING  -----------  :  ")
      check<-"warning"
    }, error=function(e){
      cat("-----------   ERROR  -----------  :   ")
      check<-"error"
    }, finally={
      cat(file_name, " / ")
    })
    
    if( check=='warning'| check=='error' ) next
    
    
    #파일 이름 변환
    names(file_data) <- c('날짜','시간대','행정동코드','집계구','총생활인구','남0',
                          paste0("남", seq(10, 70, 5)),'여0',paste0("여", seq(10, 70, 5)))
    
    #날짜 변환
    file_date <- str_sub(file_name, start=14, end=21)
    file_date <- as.Date(file_date, format = '%Y%m%d')
    
    
    #요일 추출  + 월 추출(저장 시 활용)
    day<-weekdays(file_date)
    mon<-months(file_date)
    
    #readfile 후 평일 / 주말
    if ( day %in% c("화요일,수요일,목요일") ){
      week_day <- file_data %>%
        filter(집계구 %in% dm[[3]] , 시간대 %in% c(10:20) ) %>%
        select(contains('시간'),총생활인구,집계구,contains('남'),contains('여'))
      
      #년도
      weekday_df <- rbind(weekday_df,week_day)
      #월
      mon_weekday_df <-rbind(weekday_df,week_day)
    }else if ( day %in% c("토요일","일요일") ){
      week_end <- file_data %>%
        filter(집계구 %in% dm[[3]] , 시간대 %in% c(10:20) ) %>%   
        select(contains('시간'),총생활인구,집계구,contains('남'),contains('여'))
      
      #년도
      weekend_df <- rbind(weekday_df, week_end)
      #월
      mon_weekend_df <- rbind(weekday_df, week_end)
      
      setwd('../../월별_생활인구_데이터')
      write.csv(mon_weekday_df,paste0(mon,"_평일 생활인구 데이터.csv"))
      write.csv(mon_weekend_df,paste0(mon,"_주말 생활인구 데이터.csv"))
      setwd(paste0('c:/semi-project/data/생활인구_데이터/',folder_name))
    }
  }
}

setwd('c:/semi-project/data/생활인구_데이터')
wrtie.csv(weekday_df,'전년도_종합_평일 생활인구 데이터.csv')
wrtie.csv(weekend_df,'전년도_종합_주말 생활인구 데이터.csv')





