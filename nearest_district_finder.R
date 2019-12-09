rm(list = ls())

data = read.csv("distance_matrix.csv")
data = data[-1]

header_data = read.csv("집계코드_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
options("scipen" = 100)
header = header_data[,2]

department = read.csv("점포이름_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")


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

x = department$lon[1]
y = department$lat[1]
# 
# 
# 
# x = department[header_data[,2] == my_mat[1,1], 3]
# y = header_data[header_data[,2] == my_mat[1,1],4]



library(rgdal)
library(ggplot2)

my_spdf <- readOGR( 
  dsn= paste0("../map/") , 
  layer="seoul",
  verbose=FALSE
)

target = header_data[header_data[,2] == my_mat[1,1], 2]
my_coord = my_spdf[,as.integer(my_spdf@data[, "TOT_REG_CD"]) == my_mat[1,1]]
plot(my_coord, col="#f2f2f2", bg="skyblue", border=0 )

# 'fortify' the data to get a dataframe format required by ggplot2
# library(broom)
# spdf_fortified <- tidy(my_spdf, region = "NAME")


plot(my_spdf, col="#f2f2f2", bg="skyblue", border=0 )

library(rgdal)
proj4string(my_spdf) <- CRS("+proj=longlat +ellps=WGS84")
shapefile <- spTransform(my_spdf, CRS("+proj=longlat +datum=WGS84")) #change CRS

# Plot it
fig = ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() + geom_point(aes(x,y))

fig 

ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void()

depart = department[1,]

ggplot() +
  geom_polygon(data = temp, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  geom_point(data = depart, aes(x = lon, y = lat))


ggplot()+
  geom_point(data = department, aes(lon, lat))

