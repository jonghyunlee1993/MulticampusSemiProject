rm(list = ls())

options(scipen = 100)

library(plm)
library(dplyr)

path_meta = "~/GitRepo/Multicampus_semi/JH_working/data/"
path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load pop_data
pop = read.csv(paste0(path_res, "local_pop_weekend.csv"), stringsAsFactors = F)[, -1]
pop[pop == "*"] = NA
pop[, 5:length(pop)] = apply(pop[, 5:length(pop)], 2, as.integer)

# mean visit pop의 sd가 500이 넘어가는 백화점 두 곳 제외
# 현대 신촌점, 롯데 관악점
pop_df = pop %>% group_by(date, 집계구) %>%  
  summarise(mean_pop = mean(총생활인구)) %>% 
  rename(code = 집계구) %>% 
  filter(!code %in% c(1113075030009, 1121052010001))

# load department meta data
department = read.csv(paste0(path_meta, "department.csv"), stringsAsFactors = F)

# load subway_data
subway = read.csv(paste0(path_res, "weekend_subway.csv"), stringsAsFactors = F)[, -1]
subway = subway %>% rename(subway_code = station_no) %>% 
  select(-c(station_name))

# load weather dust data
weather_dust = read.csv(paste0(path_res, "weather_dust_proc.csv"), stringsAsFactors = F)[,-1]
weather_dust[is.na(weather_dust)] = 0

# joining objects
df = left_join(pop_df, department[, 1:7], by = c("code"))
df = inner_join(df, subway, by = c("date", "subway_code"))
df = inner_join(df, weather_dust, by = "date")

# make panel dataset
# pd = pdata.frame(as.data.frame(df), index = c("date", "code"))
# pdim(pd)
# 
# within_model = plm(mean_pop ~ size + residential_area + commercial_area +
#                      green_area + pop_density + arrival + mean_temp + 
#                      mean_precipi + mean_wind + mean_snow + factor(IsRainyDay) +
#                      factor(IsDustyDay), data = pd, model = "within")
# 
# random_model = plm(mean_pop ~ size + residential_area + commercial_area +
#                      green_area + pop_density + arrival + mean_temp + 
#                      mean_precipi + mean_wind + mean_snow + factor(IsRainyDay) +
#                      factor(IsDustyDay), data = pd, model = "random")
# 
# phtest(random_model, within_model)
# 
# within_model = plm(mean_pop ~ size + residential_area + commercial_area +
#                      green_area + pop_density + arrival + mean_temp + 
#                      mean_precipi + mean_wind + mean_snow + fine_dust_grade +
#                      hyper_dust_grade, data = pd, model = "within")
# 
# summary(within_model)

# 날씨의 서울 지역 공통적 속성으로 인한 plm 적용이 어려운 문제 발견 
# 일반 선형 회귀 적용한다. 

model = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
     pop_density + arrival + mean_temp + mean_precipi + mean_wind + mean_snow +
     fine_dust_grade + hyper_dust_grade, data = df)
summary(model)

model2 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
             pop_density + arrival + mean_temp + mean_precipi + mean_wind + mean_snow +
             fine_dust + hyper_dust, data = df)
summary(model2)

model3 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
              pop_density + arrival + factor(IsDustyDay) + factor(IsRainyDay),
            data = df)
summary(model3)

model4 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
              pop_density + arrival + fine_dust_grade + hyper_dust_grade + mean_precipi,
            data = df)
summary(model4)

model5 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
              pop_density + arrival + fine_dust + hyper_dust + mean_precipi,
            data = df)
summary(model5)

model6 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
              pop_density + arrival + fine_dust + hyper_dust + mean_precipi +
              mean_temp + mean_snow + mean_wind,
            data = df)
summary(model6)

model7 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
              pop_density + arrival + factor(IsDustyDay) + factor(IsRainyDay) +
              mean_wind + mean_temp + mean_snow,
            data = df)
summary(model7)

model8 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
              pop_density + arrival + fine_dust_grade + hyper_dust_grade +
              mean_precipi + mean_temp + mean_wind + mean_snow,
            data = df)
summary(model8)

step(model8, direction = "both")

model9  = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
               pop_density + arrival + fine_dust + hyper_dust +
               mean_precipi + mean_temp,
             data = df)

step(model9, direction = "both")

model9_step = lm(formula = mean_pop ~ size + residential_area + commercial_area + 
                   green_area + pop_density + arrival + fine_dust + mean_temp, 
                 data = df)

summary(model9_step)

model10  = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
               pop_density + arrival + fine_dust_grade + hyper_dust_grade +
               mean_precipi + mean_temp,
             data = df)

step(model10, direction = "both")

model10_step = lm(formula = mean_pop ~ size + residential_area + commercial_area + 
                    green_area + pop_density + arrival + fine_dust_grade + mean_temp, 
                  data = df)

summary(model10_step)

model11 = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
               pop_density + arrival + factor(IsDustyDay) + factor(IsRainyDay) +
               mean_precipi + mean_temp,
             data = df)
step(model11, direction = "both")

###### final_model 

model9  = lm(mean_pop ~ size + residential_area + commercial_area + green_area +
               pop_density + arrival + fine_dust + hyper_dust +
               mean_precipi + mean_temp,
             data = df)

step(model9, direction = "both")


model9_step = lm(formula = mean_pop ~ size + residential_area + commercial_area + 
                   green_area + pop_density + arrival + fine_dust + mean_temp, 
                 data = df)

summary(model9_step)

plot(model9_step)

library(car)
vif(model9_step)


