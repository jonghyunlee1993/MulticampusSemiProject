# 2018

#install.packages("readxl")
library(readxl)
library(dplyr)
dust = read_excel("c:/Multicampus_semi/dust/일별평균대기오염도_2018.xlsx")
dust

head(dust)
tail(dust)

date = dust$측정일시
unique_date = unique(date)


unique_date = unlist(unique_date)

dust_avg = function(data, num){
  dust = na.omit(unlist(data[num]))
  dust_avg = mean(dust, trim = 10)
  return(dust_avg)
}

for (my_date in unique_date){
  temp = dust[dust$측정일시 == my_date, ]
  
  fine_dust_avg = dust_avg(temp, 3)
  hyper_dust_avg = dust_avg(temp, 4)
  
  # calculate find dust grade by WHO criteria
  if (fine_dust_avg >= 151){
    fine_dust_grade = 8
  }else if (fine_dust_avg >= 101){
    fine_dust_grade = 7
  }else if (fine_dust_avg >= 76){
    fine_dust_grade = 6
  }else if (fine_dust_avg >= 51){
    fine_dust_grade = 5
  }else if (fine_dust_avg >= 41){
    fine_dust_grade = 4
  }else if (fine_dust_avg >= 31){
    fine_dust_grade = 3
  }else if (fine_dust_avg >= 16){
    fine_dust_grade = 2
  }else if (fine_dust_avg >= 0){
    fine_dust_grade = 1
  }
  
  # calculate find dust grade by WHO criteria
  if (hyper_dust_avg >= 151){
    hyper_dust_grade = 8
  }else if (hyper_dust_avg >= 101){
    hyper_dust_grade = 7
  }else if (hyper_dust_avg >= 76){
    hyper_dust_grade = 6
  }else if (hyper_dust_avg >= 51){
    hyper_dust_grade = 5
  }else if (hyper_dust_avg >= 41){
    hyper_dust_grade = 4
  }else if (hyper_dust_avg >= 31){
    hyper_dust_grade = 3
  }else if (hyper_dust_avg >= 16){
    hyper_dust_grade = 2
  }else if (hyper_dust_avg >= 0){
    hyper_dust_grade = 1
  }
  
  dust_criteria = mean(fine_dust_grade, hyper_dust_grade)
  
  if (dust_criteria > 4){
    IsDustyDay = 1
  }else {
    IsDustyDay = 0
  }
}

result = data.frame(my_date, IsDustyDay)


data <- data.frame(cbind(my_date, IsDustyDay))
data
write.csv(data, file = "dust_grade_2018.csv")
