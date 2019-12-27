rm(list = ls())

url = "https://raw.githubusercontent.com/jonghyunlee1993/Multicampus_semi/master/Working/proc/final_df_for_linear_model.csv"
df = read.csv(url, fileEncoding = "UTF-8")[, -1]
df$code = as.factor(df$code)
df$IsDustyDay = as.factor(df$IsDustyDay)
df$IsRainyDay = as.factor(df$IsRainyDay)

library(nnet)

train_model = function(start, end, mape_res){
  actuals_preds = NULL
  
  test_pop = data.frame(mean_pop = df[start:end, c(3)])
  train_pop = data.frame(mean_pop = df[-c(start:end), c(3)])
  
  test_data = cbind(test_pop, as.data.frame(sapply(df[start:end, c(4, 6:10, 12:13)], scale)))
  train_data = cbind(train_pop, as.data.frame(sapply(df[-c(start:end), c(4, 6:10, 12:13)], scale)))
  
  nnet_model = nnet(log(mean_pop) ~ ., train_data, size = 50, 
                    linout = T, maxit = 100, trace = T)
  nnet_pred = predict(nnet_model, test_data)
  
  actuals_preds$actuals = log(test_pop$mean_pop)
  actuals_preds$predicteds = nnet_pred
  actuals_preds = as.data.frame(actuals_preds)
  
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) * 100
  
  
  mape_res = rbind(mape_res, as.data.frame(mape))
  
  return(mape_res)
}


data_num = nrow(df) %/% 10
mape_res = NULL

for (row_idx in 1:10){
  
  cat("iteration: ", row_idx, "\n")
  
  start = 1 + (row_idx - 1) * data_num
  end   = row_idx * data_num
  
  mape_res = train_model(start, end, mape_res)
  
}

mean_error = mean(mape_res$mape)
print(mean_error)

