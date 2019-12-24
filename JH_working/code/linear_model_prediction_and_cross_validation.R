rm(list = ls())

options(scipen = 100)
library(caret)

path_res = "~/GitRepo/Multicampus_semi/JH_working/proc/"

# load data set
load(paste0(path_res, "final_df_for_liner_model.Rdata"))
df = as.data.frame(df)
df$code = as.factor(df$code)

# train test fold
idx = createDataPartition(df$mean_pop, p = 0.8, list = F)
train_data = df[idx, ]
test_data = df[-idx, ]

table(train_data$code)
table(test_data$code)
  
model = lm(formula = log(mean_pop) ~ size + residential_area + commercial_area +
                   green_area + pop_density + arrival + fine_dust + hyper_dust,
                 data = train_data)
summary(model)

# cal MAPE error 
distPred <- predict(model, test_data)
actuals_preds <- data.frame(cbind(actuals = log(test_data$mean_pop), predicteds = distPred))
correlation_accuracy <- cor(actuals_preds)
# head(actuals_preds)
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) * 100
mape

## Corss Validation
data_num = nrow(df) %/% 10

mape_res = NULL
for (row_idx in 1:10){
  start = 1 + (row_idx - 1) * 428
  end   = row_idx * 428
  
  test_data = df[start:end, ]
  train_data = df[-c(start:end), ]
  
  model = lm(formula = log(mean_pop) ~ size + residential_area + commercial_area +
               green_area + pop_density + arrival + fine_dust + hyper_dust,
             data = train_data)
  
  distPred <- predict(model, test_data)
  actuals_preds <- data.frame(cbind(actuals = log(test_data$mean_pop), predicteds = distPred))
  correlation_accuracy <- cor(actuals_preds)
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) * 100
  mape_res = rbind(mape_res, as.data.frame(mape))
}

mean_error = mean(mape_res$mape)
print(mean_error) # average error rate 7.75%
