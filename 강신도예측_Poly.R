install.packages("readxl")
install.packages("caret")
install.packages("dplyr")
install.packages("gvlma")
install.packages("lm.beta")
library(readxl)
library(caret)
library(dplyr)
library(gvlma)
library(lm.beta)


# loda excel data
data <- readxl::read_excel(path="C:\\Users\\psc\\Desktop\\논문\\ro_ag_elongation.xlsx", sheet="Sheet1", col_names=TRUE)


r2_list = list()
mae_list = list()
mse_list = list()
cnt_list = list()


# not normalization
# repeat 1000 times
i=0
repeat{
i = i + 1
if(i > 1000) break

# split data into train and test
t_index <- sample(1:nrow(data), size=nrow(data)*0.9)
train <- data[t_index, ]
test <- data[-t_index, ]

# model
train.lm <- lm(Elongation ~ Spinbeam_temp + I(Spinbeam_temp^2) + Roller1_speed + I(Roller1_speed^2) + Roller2_speed + I(Roller2_speed^2) + Roller2_temp + I(Roller2_temp^2) + DR + I(DR^2) + FR_speed + I(FR_speed^2), data=train)
summary(train.lm)

# predict
pred <- predict(train.lm, newdata=test)

# calcuate mae, mse, r2, cnt(공정관리한계안넘은거)
b = mean(abs(test$Elongation - pred))
c = mean((test$Elongation - pred)**2)

a = abs(test$Elongation - pred)
cnt=0
for( t in a){
  if(t<=3.5){
    cnt = cnt + 1
  }
}
cnt
cnt/nrow(test)*100

pred <- as.data.frame(pred)
sst <- sum((test$Elongation - mean(test$Elongation))^2)
sse <- sum((pred - mean(test$Elongation))^2)

sst
sse

print("r2 mae mse cnt cnt")
r2_list <- append(r2_list,sse/sst)
mae_list <- append(mae_list,b)
mse_list <- append(mse_list,c)
cnt_list <- append(cnt_list,cnt)
}

# get average of some values
Reduce('+',r2_list)/length(r2_list)
Reduce('+',mae_list)/length(mae_list)
Reduce('+',mse_list)/length(mse_list)
Reduce('+',cnt_list)/length(cnt_list)
Reduce('+',cnt_list)/length(cnt_list)/nrow(test)
