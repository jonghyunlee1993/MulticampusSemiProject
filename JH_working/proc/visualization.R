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

cen <- c(mean(departments$lon), mean(departments$lat))

register_google(key='AIzaSyD8k2DWC_7yFHCrH6LDR3RfITsmWMEqC8c')
map <- get_googlemap(center = cen,
                     maptype = "roadmap",
                     zoom = 11)
# theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
# theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요

ggmap(map) +
  geom_point(data = departments, aes(colour = 회사명), size = 4, alpha = 0.5) +
  scale_colour_manual(values = c('red','blue','green')) +
  theme(legend.title = theme.leti, legend.text = theme.lete)
  # theme(legend.text = element_text(size = 16),
  #       legend.position = "bottom")
  # geom_label(data = departments, label = departments$name, nudge_x = 0.04, alpha = 0, label.size = NA) + 
  # geom_text(data = departments, aes(label = departments$name)) +
  # theme(text = element_text(family = "NanumGothic"))
  
# 한글 깨짐 긴급 조치
library(extrafont)
font_import()
fonts()
