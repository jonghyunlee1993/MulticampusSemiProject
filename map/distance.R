# install.packages("geosphere")

setwd("c:\\semi-project\\data")
depart = read.csv("점포이름_위도_경도_주소.csv", stringsAsFactors=F)
counting_code = read.csv("집계코드_위도_경도_주소.csv", stringsAsFactors=F)
depart_name = read.csv("백화점주소_2019년.csv", stringsAsFactors=F)

library(geosphere)
result=c()
for (i in 1:25) {
  d1=depart$lon[i]
  d2=depart$lat[i]
  
  for (j in 1:19062){
    c1=counting_code$lon[j]
    c2=counting_code$lat[j]
    
    di=distm (c(d1, d2), c(c1, c2), fun = distHaversine)
    di=as.numeric(di)
    di=as.matrix(di)
    result=cbind(result,di)
  }
}

result = matrix(result, ncol  = 25)
row.names(result) = c(counting_code$name)

min.result = apply(result,2,min)
min.result = as.matrix(min.result)
min.result = cbind(depart_name$name,depart_name$subname,min.result)

write.csv(result,'백화점별 집계구와의 거리.csv')
write.csv(min.result,'백화점별 집계구역 최소거리.csv')


