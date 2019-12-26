rm(list = ls())

options(scipen = 100)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load data set
load(paste0(path_res, "final_df_for_liner_model.Rdata"))
df = as.data.frame(df)
df$code = as.factor(df$code)

model = rpart(log(mean_pop) ~ size + residential_area + commercial_area +
               green_area + pop_density + arrival + fine_dust + hyper_dust +
               mean_temp + mean_precipi + mean_wind + mean_snow, 
             data = df, control = rpart.control(minsplit = 2))
rpart.plot(model)

model2 = rpart(log(mean_pop) ~ size + residential_area + commercial_area +
                green_area + pop_density + arrival + fine_dust + hyper_dust +
                mean_temp + mean_precipi + mean_wind + mean_snow, 
              data = df)
rpart.plot(model2)


random_model = randomForest(log(mean_pop) ~ size + residential_area + commercial_area +
                              green_area + pop_density + arrival + fine_dust + hyper_dust +
                              mean_temp + mean_precipi + mean_wind + mean_snow,
                            data = df, importance = T)
pred = predict(random_model, df)

mape <- mean(abs((pred - log(df$mean_pop)))/log(df$mean_pop)) * 100
mape


