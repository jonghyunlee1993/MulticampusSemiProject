rm(list = ls())

data = read.csv("distance_matrix.csv")
data = data[-1]

header = read.csv("집계코드_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
options("scipen" = 100)
header[,2]

sum(data == 0)

# my_mat = matrix(rep(0,length(data)*5), nrow=5)
my_mat = NULL

for (dept in 1:length(data)){
  dat = data[,dept]
  dat[dat == 0] = NA
  my_mat = cbind(my_mat, head(sort(dat)))
  print(my_mat[,dept])
}

