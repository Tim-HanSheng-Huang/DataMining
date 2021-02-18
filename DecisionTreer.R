
winequality <- read.csv("C:/Users/user/Desktop/DM_Assignment_4_winequality_red.csv")
str(winequality)

winequality[,"quality"] <- as.factor(winequality[,"quality"])

require(rpart)

cart.model<- rpart(quality ~. , 
                   data=winequality)

cart.model


require(rpart.plot) 
prp(cart.model,
    faclen=0,           
    fallen.leaves=TRUE, 
    shadow.col="gray",  
    extra=2)

printcp(cart.model)
plotcp(cart.model)

#­pºâ·Ç½T²v
pred <- predict(cart.model, newdata=winequality, type="class")
table(real=winequality$quality, predict=pred)
confus.matrix <- table(real=winequality$quality, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix)

