rm(list = ls())

library(ggplot2)
library(dplyr)

path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

theme.ti <- element_text(family="NanumGothic", face="bold", size=12)
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5)

# load pop_data
pop = read.csv(paste0(path_res, "local_pop_weekend.csv"), stringsAsFactors = F)[, -1]
pop[pop == "*"] = NA
pop[, 5:length(pop)] = apply(pop[, 5:length(pop)], 2, as.integer)

# load department data
department = read.csv(paste0(path_meta , "department.csv"))

# 몇몇 백화점의 히스토그램이 정규 분포를 따르지 않음
pop_df = pop %>% group_by(date, 집계구) %>%  
  summarise(mean_pop = mean(총생활인구)) %>% 
  rename(code = 집계구)

codes = c(1119074050002, 1117051030003, 1125073030012,1123058040002, 1123051010201, 1113075030010,
          1115051010004, 1108068010004, 1124080020103, 1102052020001, 1111066030001, 1123063020021,
          1105066011201, 1121052010001, 1124077030001, 1109071040007)

show_hist = function(code){
  X = pop_df[pop_df$code == code, ]
  
  # print(paste("sd:", sd(X$mean_pop)))
  
  fig = ggplot(data = X, aes(x = mean_pop)) +
    geom_histogram(fill = "lightblue", colour = "black", bins = 10) +
    ggtitle(paste("집계구", code, department[codes == code, ]$name)) +
    labs(x = "일평균 생활인구", y = "횟수") +
    theme(plot.title = theme.ti,
          axis.title = theme.ax)
  
  print(fig)
}

for (dept_code in codes){
  show_hist(dept_code)
}

# 관악 이상치 가능성 112105010001