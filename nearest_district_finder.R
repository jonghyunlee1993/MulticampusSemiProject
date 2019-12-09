rm(list = ls())

data = read.csv("distance_matrix.csv")
data = data[-1]

header_data = read.csv("집계코드_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
options("scipen" = 100)
header = header_data[,2]

# sum(data == 0)
# my_mat = matrix(rep(0,length(data)*5), nrow=5)
my_mat = NULL

for (dept in 1:length(data)){
  dat = data[,dept]
  dat[dat == 0] = NA
  
  idx = order(dat)
  
  my_mat = cbind(my_mat, head(header[idx]), head(sort(dat)))
  print(my_mat[,dept])
}



header_data[header_data[,2] == my_mat[1,1],3]
header_data[header_data[,2] == my_mat[1,1],4]
