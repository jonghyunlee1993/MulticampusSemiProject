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

show_hist = function(code){
  X = pop_df[pop_df$code == code, ]
  
  # print(paste("sd:", sd(X$mean_pop)))
  
  fig = ggplot(data = X, aes(x = mean_pop)) +
    geom_histogram(fill = "lightblue", colour = "black", bins = 10) +
    ggtitle(paste("집계구", code, department[department$code == code, ]$name)) +
    labs(x = "일평균 생활인구", y = "횟수") +
    theme(plot.title = theme.ti,
          axis.title = theme.ax)
  
  print(fig)
}

# 신촌
show_hist(1113075030009)

# 목동 
show_hist(1115051010002)

# 관악 
show_hist(1121052010001)

# 강남
show_hist(1123063020012)

# noise = c(1113075030009, 1121052010001, 1121052010001, 1123063020012)

# 영등포 
show_hist(1119074050006)

for (dept_code in department$code){
  show_hist(dept_code)
}
