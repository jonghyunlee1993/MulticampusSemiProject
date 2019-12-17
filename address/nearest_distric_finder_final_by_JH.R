rm(list = ls())

setwd("~/GitRepo/Multicampus_semi/address")

header_data = read.csv("집계코드_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
options("scipen" = 100)
header = header_data[,2]
head(header_data)

department = read.csv("점포이름_위도_경도_주소.csv", fileEncoding = "CP949", encoding = "euc-kr")
head(department)

library(rgdal)
library(ggplot2)

my_spdf <- readOGR( 
  dsn= paste0("../map/") , 
  layer="seoul"
)

# 한국 중부 좌표계
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs")
# from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m")
# 세계 표준 좌표계
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(my_spdf) = from_crs

converted = spTransform(my_spdf, to_crs)

library(broom)
shp_df = tidy(converted)
district_data = aggregate(cbind(long, lat) ~ id, data = shp_df, FUN = mean)
district_data = cbind(TOT_REG_CD = converted$TOT_REG_CD, district_data)

library(geosphere)
my_dist = function(lon1, lat1, lon2, lat2){
  dist = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)[1,1]
  return(dist)
}

res = matrix(rep(0, length(district_data$TOT_REG_CD) * length(department$name)), nrow = length(department$name))

for (depart in 1:length(department$name)){
  # for (depart in 1:1){
  for (addr in 1:length(district_data$TOT_REG_CD)){
    # if (depart * addr %% 5000 == 1) 
    # {
    #   cat(addr * depart / length(district_data$TOT_REG_CD) * length(department$name),"%\n")
    # }
    res[depart, addr] = my_dist(department$lon[depart], department$lat[depart], district_data$long[addr], district_data$lat[addr])
    # print(my_dist(department$lon[depart], department$lat[depart], address$lon[addr], address$lat[addr]))
  }
}

res = t(res)
write.csv(res, "distance_matrix_with_shp.csv")


my_mat = NULL
for (dept in 1:ncol(res)){
  dat = res[,dept]
  # dat[dat == 0] = NA
  
  idx = order(dat)
  
  my_mat = cbind(my_mat, head(header[idx]), head(sort(dat)))
}

colnames(my_mat) = rep(as.character(department$name), each = 2)
View(my_mat)

for (depart in 1:nrow(department)){
  target = header_data[header_data[,2] %in% my_mat[, (depart * 2 - 1)], 2]
  # target_coord = my_spdf[as.integer(my_spdf@data[, "TOT_REG_CD"]) == target, ]
  # plot(target_coord)
  
  target_coord = subset(converted, TOT_REG_CD %in% target)
  target_df = broom::tidy(target_coord)
  cname = aggregate(cbind(long, lat) ~ id, data = target_df, FUN = mean)
  label_data = cbind(cname, target_coord@data)
  
  my_plot = ggplot() +
    geom_polygon(data = target_coord, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
    theme_gray(base_family='AppleMyungjo') + 
    geom_point(data = department[depart,], aes(lon, lat)) +
    geom_text(data = label_data, aes(x = long, y = lat, label = TOT_REG_CD), 
              check_overlap = F, angle = 45) +
    ggtitle(department$name[depart])
  
  print(my_plot)
  
  invisible(readline(prompt="Press [enter] to continue"))
}
