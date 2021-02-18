
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
#                data=train,           # ���
#                method="neuralnet",   # �����g����(bpn)
# 
#                # �̭��n���B�J�G�[��P�ƦC�զX(�Ĥ@�h1~4��nodes ; �ĤG�h0~4��nodes)
#                # �ݦ�رƦC�զX(�h�����üh�B�C�h�h�֭�node)�A�|���̤p��RMSE
#                tuneGrid = expand.grid(.layer1=c(1:10), .layer2=c(0:10), .layer3=c(0:10)),
# 
#                # �H�U���ѼƳ]�w�A�M�W����neuralnet���@��
#                learningrate = 0.1,  # learning rate
#                threshold = 0.5,     # partial derivatives of the error function, a stopping criteria
#                stepmax = 5e5         # �̤j��ieration�� = 500000(5*10^5)
# )
# 
# # �|�i�D�A�̨Ϊ��ѼƲզX�O����G�Ĥ@���üh1��node�A�ĤG���üh2��node
# model

######################
ptm <- proc.time()
bpn <- neuralnet(formula = formula.bpn,
                 data = trainoftrain,
                 hidden = c(1,2,1),       
                 learningrate = 0.1, # learning rate
                 threshold = 0.1,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e6        # �̤j��ieration��
)
proc.time() - ptm

plot(bpn)


###########################
pred<- compute(bpn,testoftrain[,2:5])
predict_value_y1=pred$net.result[,1:1]
actual_value_y1=testoftrain[,6:6]
predict_value_y2=pred$net.result[,2:2]
actual_value_y2=testoftrain[,7:7]

#�p��RMSE
sqrt( sum( (predict_value_y1 - actual_value_y1)^2 , na.rm = TRUE ) / length(actual_value_y1) )#Y1
sqrt( sum( (predict_value_y2 - actual_value_y2)^2 , na.rm = TRUE ) / length(actual_value_y2) )#Y2




#�w��TEST
pred <- compute(bpn_3, test[,2:5]) 
pred$net.result
pred.result <- as.data.frame(pred)
pred.result

write.table(pred.result,file="C:/Users/user/Desktop/DM_Assignment_4_NNOutcome.csv",sep=",",row.names=F, na = "NA")
