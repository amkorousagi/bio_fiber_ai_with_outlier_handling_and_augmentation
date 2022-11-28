install.packages("readxl")
install.packages("dplyr")
install.packages("FNN")
install.packages("caret")
devtools::install_github ("https://github.com/cran/e1071")

library(readxl)
library(FNN)
library(e1071)
library(caret)

# min-max normalization
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# load excel data
# you can load any excel data in this directory
data <- readxl::read_excel(path="C:\\Users\\psc\\Desktop\\논문\\ro_ag_elongation.xlsx", sheet="Sheet1", col_names=TRUE)

# drop meanless column and normlize
data[2:7] <- lapply(data[2:7], normalize) 

# declare list for averages of some values  
r2_list = list()
mae_list = list()
mse_list = list()
cnt_list = list()

r2_list2 = list()
mae_list2 = list()
mse_list2 = list()
cnt_list2 = list()

# execute knn 10 times
# if your target is Tenacity, convert Elongation to Tenacity in this code
i=0
repeat{
  i = i + 1
  if(i > 10) break

  # split data into train data and test data
  t_index <- sample(1:nrow(data), size=nrow(data)*0.9)
  train <- data[t_index, ]
  test <- data[-t_index, ]
  
  x_te.train <- model.matrix(Elongation ~ Spinbeam_temp + Roller1_speed + Roller2_speed + Roller2_temp + DR + FR_speed, data=train)[ ,-1]
  y_te.train <- train$Elongation
  
  x_te.test <- model.matrix(Elongation ~ Spinbeam_temp + Roller1_speed + Roller2_speed + Roller2_temp + DR + FR_speed, data=test)[ ,-1]
  y_te.test <- test$Elongation
  
  k_list <- vector()
  pf_list <- vector()
  
  # repeate 1000 times for best parameter
  for(i in 1:1000){
    tune.out <- tune.knn(x=x_te.train, y=as.factor(y_te.train), k=1:20)
    k_list[i] <- tune.out$best.parameters$k
    pf_list[i] <- tune.out$best.performance
    print(".")
  }
  
  # predict with k = 1, 2
  
  fit_te_k1 <- caret::knnreg(x_te.train, y_te.train, k=1)
  fit_te_k2 <- caret::knnreg(x_te.train, y_te.train, k=2)
  
  pred_te_k1 <- predict(fit_te_k1, x_te.test)
  pred_te_k2 <- predict(fit_te_k2, x_te.test)
  

  postResample(pred_te_k1, test$Elongation)
  postResample(pred_te_k2, test$Elongation)
  

  # calculate mae, mse, r^2
  # count the number of results within the criterion(공정관리한계) 

  mae_k1 = mean(abs(test$Elongation - pred_te_k1))
  mae_k2 = mean(abs(test$Elongation - pred_te_k2))
  
  mse_k1 <- mean((pred_te_k1 - test$Elongation)^2)
  mse_k2 <- mean((pred_te_k2 - test$Elongation)^2)
  
  a = abs(test$Elongation - pred_te_k1)
  cnt=0
  for( t in a){
    if(t<=3.5){
      cnt = cnt + 1
    }
  }
  
  pred_te_k1 <- as.data.frame(pred_te_k1)
  sst <- sum((test$Elongation - mean(test$Elongation))^2)
  sse <- sum((pred_te_k1 - mean(test$Elongation))^2)
  
  r2_list <- append(r2_list,sse/sst)
  mae_list <- append(mae_list,mae_k1)
  mse_list <- append(mse_list,mse_k1)
  cnt_list <- append(cnt_list,cnt)
  
  
  a = abs(test$Elongation - pred_te_k2)
  cnt=0
  for( t in a){
    if(t<=3.5){
      cnt = cnt + 1
    }
  }
  
  pred_te_k2 <- as.data.frame(pred_te_k2)
  sst <- sum((test$Elongation - mean(test$Elongation))^2)
  sse <- sum((pred_te_k2 - mean(test$Elongation))^2)
  
  r2_list2 <- append(r2_list2,sse/sst)
  mae_list2 <- append(mae_list2,mae_k2)
  mse_list2 <- append(mse_list2,mse_k2)
  cnt_list2 <- append(cnt_list2,cnt)
}

# get average for list of some values

Reduce('+',r2_list)/length(r2_list)
Reduce('+',mae_list)/length(mae_list)
Reduce('+',mse_list)/length(mse_list)
Reduce('+',cnt_list)/length(cnt_list)
Reduce('+',cnt_list)/length(cnt_list)/nrow(test)

Reduce('+',r2_list2)/length(r2_list2)
Reduce('+',mae_list2)/length(mae_list2)
Reduce('+',mse_list2)/length(mse_list2)
Reduce('+',cnt_list2)/length(cnt_list2)
Reduce('+',cnt_list2)/length(cnt_list2)/nrow(test)
