rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/address")

data = read.csv("distance_matrix.csv")
data = data[-1]

header_data = read.csv("집계코드_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
options("scipen" = 100)
header = header_data[,2]
head(header_data)

department = read.csv("점포이름_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
head(department)

my_mat = NULL

for (dept in 1:length(data)){
  dat = data[,dept]
  dat[dat == 0] = NA
  
  idx = order(dat)
  
  my_mat = cbind(my_mat, head(header[idx]), head(sort(dat)))
}

library(rgdal)
library(ggplot2)

my_spdf <- readOGR( 
  dsn= paste0("../map/") , 
  layer="seoul"
)

# 한국 중부 좌표계
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m")
# 세계 표준 좌표계
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(my_spdf) = from_crs

converted = spTransform(my_spdf, to_crs)
# converted

plot(converted, axes = T)

# target = header_data[header_data[,2] == my_mat[1,1], 2]
# my_coord = my_spdf[,as.integer(my_spdf@data[, "TOT_REG_CD"]) == my_mat[1,1]]

library(dplyr)
for (i in department$name) {
  if (substr(i, 1, 2) == "신세") {
    
  }
}

department = department %>% mutate(comp = ifelse(substr(name, 1, 2) == "신세", 1, 
                                    ifelse(substr(name, 1, 2) == "현대", 2, 3)))


library(rgdal)

# Plot it
fig = ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() + geom_point(aes(x,y))

fig 

ggplot() +
  geom_polygon(data = converted, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() + geom_point(data = department, aes(lon, lat, colour = factor(comp)))

# depart = department[1,]
# 
# ggplot() +
#   geom_polygon(data = temp, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   geom_point(data = depart, aes(x = lon, y = lat))
# 
# 
# ggplot()+
#   geom_point(data = department, aes(lon, lat))

