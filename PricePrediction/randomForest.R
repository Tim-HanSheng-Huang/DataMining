
data_set <- read.csv("E:/DM/final/data/DC_imputeNA_rmoutlier_dummy.csv")
data_set <- subset(data_set, select = -c(ZIPCODE) )

data_setx <- subset(data_set, select = -c(PRICE) )
data_sety <- subset(data_set, select = c(PRICE) )
data_setx <- as.data.frame(data_setx)
data_sety <- as.data.frame(data_sety)


set.seed(30)
train.index <- sample(x=1:nrow(data_set), size=ceiling(0.8*nrow(data_set) ))

train = data_set[train.index, ]
test = data_set[-train.index, ]



install.packages("ranger")
require(rpart)
require(randomForest)
library(randomForestSRC)
library(ranger)


#非CV之預測
rf <- randomForest(data = train ,PRICE~.,mtry = 19,  ntree = 20,nodesize = 50)
pred <- predict(rf,newdata = test)
(mean((pred - test$PRICE)^2))^(1/2)


varImpPlot(rf)
summary(rf)
plot(rf)
abline(v=50,lwd=2,lty=2,col=2)

#找出最佳參數
hyper_grid <- expand.grid(
  mtry = seq(5, 20, by = 2),
  node_size = seq(10, 100, by = 10),
  ntree=seq(10,100,by=10),
  OOB_RMSE = 0
)

# 組合數
nrow(hyper_grid)

for (i in 1:nrow(hyper_grid)) {
  # train model
  model <- ranger(
    formula = PRICE ~ .,
    data = train, 
    num.trees = 50, 
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i], 
    
    seed = 123
  )
  
  # 並將每一此訓練模型的OOB RMSE萃取儲存
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

# 將結果依序OOB_RMSE由小至大排列，取模型成效前十名印出
hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>% 
  head(10)


r<-sample(x=1:nrow(data_set), size=ceiling(nrow(data_set) ))
data_set1 = data_set[r, ]
#做CV5
all <- 0
row<-nrow(data_set1) 
for (i in c(1:5)){
  a <- row*(0.2*(5-i))+1
  b <- row*(0.2*(5-i+1))
  tra1 <- data_set1[1:a,]
  tra2 <- data_set1[b:row,]
  tra <- rbind(tra1,tra2)
  dim(tra)
  tes <- data_set1[a:b,]
  rf <- randomForest(data = tra ,PRICE~.,mtry = 19,  ntree = 20,nodesize = 50)
  pred <- predict(rf,newdata = tes)
  rmse <- (mean((pred - tes$PRICE)^2))^(1/2)
  print(rmse)
  all <- rmse + all
  
}

(all/5)
