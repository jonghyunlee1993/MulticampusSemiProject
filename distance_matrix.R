require(rgdal)
setwd("C:/Users/student/Desktop/seoul")
shape <- readOGR(dsn = ".", layer = "my_map")
plot(shape)

setwd("C:\\Semi_project\\address")


library(geosphere)
address = read.csv("집계코드_위도_경도_주소.csv", header = T)
department = read.csv("점포이름_위도_경도_주소.csv", header = T)

my_dist = function(lon1, lat1, lon2, lat2){
  dist = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)[1,1]
  return(dist)
}

data = matrix(rep(0, length(address$X) * length(department$X)), nrow = length(department$X))

for (depart in 1:length(department$X)){
# for (depart in 1:1){
  for (addr in 1:length(address$X)){
    if (length(address$X) * length(department$X) %% 100 == 1) {cat(addr * depart / length(address$X) * length(department$X),"%")}
    data[depart, addr] = my_dist(department$lon[depart], department$lat[depart], address$lon[addr], address$lat[addr])
    # print(my_dist(department$lon[depart], department$lat[depart], address$lon[addr], address$lat[addr]))
  }
}

temp = t(data)
res = as.data.frame(temp)
write.csv(res, "distance_matrix.csv")
