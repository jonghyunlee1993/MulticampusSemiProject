rm(list = ls())

library(ggplot2)
library(ggmap)
library(dplyr)

path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
data = read.csv(paste0(path_meta, "department.csv"), stringsAsFactors = F)

departments = data %>% 
  select(company, name, lon = lon, lat = lat) %>% 
  mutate(comp_factor = ifelse(company == "신세계", 1, 
                              ifelse(company == "현대", 2, 3))) %>% 
  rename(회사명 = company)
  as.data.frame

# cen <- c(mean(departments$lon), mean(departments$lat))
# cen = c(127.0016985, 37.5642135)
cen = c(126.98, 37.5642135)

register_google(key='AIzaSyD8k2DWC_7yFHCrH6LDR3RfITsmWMEqC8c')
map <- get_googlemap(center = cen,
                     maptype = "roadmap",
                     zoom = 11)

library(rgdal)
my_spdf <- readOGR(dsn = "~/GitRepo/Multicampus_semi/map/", layer = "seoul")

# 한국 중부 좌표계
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m")
# 세계 표준 좌표계
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(my_spdf) = from_crs
converted = spTransform(my_spdf, to_crs)

theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요

theme_update(text = element_text(family = "NanumGothic"))
ggmap(map) +
  geom_polygon(data = converted, aes(x = long, y = lat, group = group), alpha = 0.6, fill='lightblue') +
  geom_point(data = departments, aes(colour = 회사명), size = 4, alpha = 0.5) +
  # scale_color_discrete(breaks = c("롯데", "현대", "신세계")) +
  scale_colour_manual(values = c("롯데" = 'red', "현대" = "blue", "신세계" = 'yellow')) +
  ggtitle("분석 대상 서울 소재 백화점") +
  labs(x = "경도", y = "위도") +
  # geom_text(data = departments, aes(label = name)) +
  theme(plot.title = theme.ti,
        axis.title = theme.ax,
        legend.title = theme.leti, 
        legend.text = theme.lete, 
        legend.position = c(0.88, 0.16))
  # theme(legend.text = element_text(size = 16),
  #       legend.position = "bottom")
  # geom_label(data = departments, label = departments$name, nudge_x = 0.04, alpha = 0, label.size = NA) + 
  # geom_text(data = departments, aes(label = departments$name)) +
  # theme(text = element_text(family = "NanumGothic"))
  
# 한글 깨짐 긴급 조치
library(extrafont)
font_import()
fonts()
