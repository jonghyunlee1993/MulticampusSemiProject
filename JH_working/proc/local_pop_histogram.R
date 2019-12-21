rm(list = ls())

load("local_pop_weekend.Rdata")
data = res

library(dplyr)
show_hist = function(code){
  data = data %>% filter(집계구 == code) %>% 
    group_by(date) %>% 
    summarise(mean_visit = mean(총생활인구))
  
  print(hist(data$mean_visit, main = paste("Hist of", code)))
}

codes = unique(data$집계구)

for (code in codes){
  show_hist(code)
}
