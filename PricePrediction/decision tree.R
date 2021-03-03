data <- read.csv("E:/DM/final/data/DC_imputeNA_rmoutlier_dummy.csv")
library(randomForestSRC)
library(rpart)
library(rpart.plot)
require(caret)
data <- subset(data, select = -c(ZIPCODE) )
data$AYB <- as.factor(data$AYB)


train.index <- sample(nrow(data),0.8*nrow(data))
train <- data[train.index,]
test <- data[-train.index,]


tree <- rpart(data = train, PRICE~. ,control=rpart.control( cp=0.044))
rpart.plot(tree)

pred <- predict(tree, newdata=test)
(mean((pred - test$PRICE)^2))^(1/2)

  
printcp(tree) # display the results 
plotcp(tree) # visualize cross-validation results 
summary(tree) # detailed summary of splits



train_control <- trainControl(method="cv", number=5)
train_control.model <- train(PRICE ~., data=train, method="rpart", trControl=train_control)
train_control.model
