require(xgboost)

data_set <- read.csv("E:/DM/final/data/DC_imputeNA_rmoutlier_dummy.csv")

set.seed(30)
train.index <- sample(x=1:nrow(data_set), size=ceiling(0.8*nrow(data_set) ))

train = data_set[train.index, ]
test = data_set[-train.index, ]

train_data <- subset(train, select = -c(PRICE) )
train_label <- subset(train, select = c(PRICE) )

test_data <- subset(test, select = -c(PRICE) )
test_label <- subset(test, select = c(PRICE) )

data_set_data <- subset(data_set, select = -c(PRICE) )
data_set_label <- subset(data_set, select = c(PRICE) )


dtrain = xgb.DMatrix(data = as.matrix(train_data),
                     label = train_label$PRICE)
dtest = xgb.DMatrix(data = as.matrix(test_data),
                    label = test_label$PRICE)
ddata_set = xgb.DMatrix(data = as.matrix(data_set_data),
                     label = data_set_label$PRICE)


summary(test_label$PRICE)
str(train_data)

xgb.params = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.8,                    
  # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  subsample = 0.8,                      
  booster = "gbtree",
  # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 20,           
  # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.03,
  # 或用'mae'也可以
  eval_metric = "rmse",                      
  objective = "reg:linear",
  # 越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0)        


cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=3000,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 23 # 每20個單位才顯示一次結果，
) 



tmp = cv.model1$evaluation_log
#畫出rmse圖
plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
abline(v=70,lwd=2,lty=2)
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )


start.time <- Sys.time()
# 4. 用xgb.train()建立模型
xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = 70)

end.time <- Sys.time()
taken <- end.time - start.time
taken
# 畫出 xgb 內的所有決策樹
# xgb.plot.tree(model = xgb.model) 


xgb.save(xgb.model, 'xgboost_rmoutlier.model')
#bst = xgb.load('xgboost.model')

# 預測
xgb_y_test = predict(xgb.model, dtest)
(mean((xgb_y_test - test$PRICE)^2))^(1/2) # rMSE

#跑有測試資料的rmse，並且選擇70那筆
cv.model1 = xgb.cv(
  params = xgb.params, 
  data = ddata_set,
  nfold = 5,     
  nrounds=70,   
  early_stopping_rounds = 30, 
  print_every_n = 23 
) 



alldata <-cbind(test_label$PRICE,xgb_y )
write.csv(alldata, file = "E:/DM/final/data/xgbPredict_rmoutlier.csv", row.names=FALSE)
#解釋變數
imp = xgb.importance(names(train_data),model=xgb.model)
print(imp)
View(imp)
library(Ckmeans.1d.dp)
xgb.plot.importance(imp,top_n = 20)
library(DiagrammeR)
xgb.plot.tree(feature_names=names(train_data),model=xgb.model, n_first_tree=2)

write.csv(imp, file = "E:/DM/final/data/var_rmoutlier.csv", row.names=FALSE)