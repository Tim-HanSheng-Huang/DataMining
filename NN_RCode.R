
NNData <- read.csv("C:/Users/user/Desktop/DM_Assignment_4_NN.csv")

str(NNData)

train=NNData[1:280,]
test=NNData[281:300,]

require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters

formula.bpn <- y1 + y2  ~ x1 + x2 + x3 + x4



smp.size <- floor(0.8*nrow(train)) 
set.seed(123)                     
train.ind <- sample(seq_len(nrow(train)), smp.size)
trainoftrain <- train[train.ind, ]
testoftrain <- train[-train.ind, ]

# # tune parameters
# model <- train(form=formula.bpn,     # formula
#                data=train,           # 資料
#                method="neuralnet",   # 類神經網路(bpn)
# 
#                # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
#                # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
#                tuneGrid = expand.grid(.layer1=c(1:10), .layer2=c(0:10), .layer3=c(0:10)),
# 
#                # 以下的參數設定，和上面的neuralnet內一樣
#                learningrate = 0.1,  # learning rate
#                threshold = 0.5,     # partial derivatives of the error function, a stopping criteria
#                stepmax = 5e5         # 最大的ieration數 = 500000(5*10^5)
# )
# 
# # 會告訴你最佳的參數組合是什麼：第一隱藏層1個node，第二隱藏層2個node
# model

######################
ptm <- proc.time()
bpn <- neuralnet(formula = formula.bpn,
                 data = trainoftrain,
                 hidden = c(1,2,1),       
                 learningrate = 0.1, # learning rate
                 threshold = 0.1,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e6        # 最大的ieration數
)
proc.time() - ptm

plot(bpn)


###########################
pred<- compute(bpn,testoftrain[,2:5])
predict_value_y1=pred$net.result[,1:1]
actual_value_y1=testoftrain[,6:6]
predict_value_y2=pred$net.result[,2:2]
actual_value_y2=testoftrain[,7:7]

#計算RMSE
sqrt( sum( (predict_value_y1 - actual_value_y1)^2 , na.rm = TRUE ) / length(actual_value_y1) )#Y1
sqrt( sum( (predict_value_y2 - actual_value_y2)^2 , na.rm = TRUE ) / length(actual_value_y2) )#Y2




#預測TEST
pred <- compute(bpn_3, test[,2:5]) 
pred$net.result
pred.result <- as.data.frame(pred)
pred.result

write.table(pred.result,file="C:/Users/user/Desktop/DM_Assignment_4_NNOutcome.csv",sep=",",row.names=F, na = "NA")

