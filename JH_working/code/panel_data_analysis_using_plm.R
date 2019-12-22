rm(list = ls())

options(scipen = 100)

library(plm)
library(dplyr)

path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load pop_data
pop = read.csv(paste0(path_res, "local_pop_weekend.csv"), stringsAsFactors = F)[, -1]
pop$집계구 = as.factor(df$집계구)
pop[pop == "*"] = NA
pop[, 5:length(pop)] = as.integer(pop[, 5:length(pop)])
pop[, 5:length(pop)] = apply(pop[, 5:length(pop)], 2, as.integer)

pop_df = pop %>% group_by(date, 집계구) %>%  
  summarise(mean_pop = mean(총생활인구)) %>% 
  rename(code = 집계구)

# load department meta data
department = read.csv(paste0(path_meta, "department.csv"), stringsAsFactors = F)

# load subway_data
subway = read.csv(paste0(path_res, "weekend_subway.csv"), stringsAsFactors = F)[, -1]
subway = subway %>% rename(subway_code = station_no) %>% 
  select(-c(station_name))

# joining objects
df = left_join(pop_df, department[, 1:7], by = c("code"))
df = inner_join(df, subway, by = c("date", "subway_code"))

# make panel dataset
pd = pdata.frame(pop_df, index = c("date", "code"))
pdim(pd)
      