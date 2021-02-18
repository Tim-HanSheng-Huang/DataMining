
Kidney_data <- read.csv("C:/Users/user/Desktop/DM_Assignment_2_Kidney.csv",na.strings= c("","?"))

head(Kidney_data,n=5)
str(Kidney_data)
Kidney_data$sg <- factor(Kidney_data$sg)
Kidney_data$al <- factor(Kidney_data$al)
Kidney_data$su <- factor(Kidney_data$su)
str(Kidney_data)
summary(Kidney_data)

#�ƭȸ�ƪ�variance
Kidney_data$age
var(Kidney_data$age,na.rm=TRUE)
var(Kidney_data$bp,na.rm=TRUE)
var(Kidney_data$bgr,na.rm=TRUE)
var(Kidney_data$bu,na.rm=TRUE)
var(Kidney_data$sc,na.rm=TRUE)
var(Kidney_data$sod,na.rm=TRUE)
var(Kidney_data$pot,na.rm=TRUE)
var(Kidney_data$hemo,na.rm=TRUE)
var(Kidney_data$pcv,na.rm=TRUE)
var(Kidney_data$wc,na.rm=TRUE)
var(Kidney_data$rc,na.rm=TRUE)

#�ƭȸ�ƪ�data distribution
hist(Kidney_data$age,main=colnames(Kidney_data["age"]),xlab=colnames(Kidney_data["age"]))
hist(Kidney_data$bp,main=colnames(Kidney_data["bp"]),xlab=colnames(Kidney_data["bp"]))
hist(Kidney_data$bgr,main=colnames(Kidney_data["bgr"]),xlab=colnames(Kidney_data["bgr"]))
hist(Kidney_data$bu,main=colnames(Kidney_data["bu"]),xlab=colnames(Kidney_data["bu"]))
hist(Kidney_data$sc,main=colnames(Kidney_data["sc"]),xlab=colnames(Kidney_data["sc"]))
hist(Kidney_data$sod,main=colnames(Kidney_data["sod"]),xlab=colnames(Kidney_data["sod"]))
hist(Kidney_data$pot,main=colnames(Kidney_data["pot"]),xlab=colnames(Kidney_data["pot"]))
hist(Kidney_data$hemo,main=colnames(Kidney_data["hemo"]),xlab=colnames(Kidney_data["hemo"]))
hist(Kidney_data$pcv,main=colnames(Kidney_data["pcv"]),xlab=colnames(Kidney_data["pcv"]))
hist(Kidney_data$wc,main=colnames(Kidney_data["wc"]),xlab=colnames(Kidney_data["wc"]))
hist(Kidney_data$rc,main=colnames(Kidney_data["rc"]),xlab=colnames(Kidney_data["rc"]))


final_class <- Kidney_data["class"]
sum(is.na(final_class))

#��class�ONA�Ȫ��h��
rm_Kidney_data <- Kidney_data[complete.cases(Kidney_data["class"]), ]
sum(is.na(rm_Kidney_data["class"] )) #���s�p��NA�ƶq


#��KNN�Ӷ�ɿ򥢭�
#install.packages("DMwR")
require(DMwR)
knn_rm_Kidney_data <- knnImputation(rm_Kidney_data)
head(knn_rm_Kidney_data)
sum(is.na(knn_rm_Kidney_data))


#��DUMMY
#install.packages("dummies")
require(dummies)
dummy_knn_rm_Kidney_data <- dummy.data.frame(knn_rm_Kidney_data)
head(dummy_knn_rm_Kidney_data)
str(dummy_knn_rm_Kidney_data)


#�P�_���s��
require(datasets)  
outlier_data <- rm_Kidney_data
outlier_data <- subset(outlier_data,select = -c(sg,al,su,rbc,pc,pcc,ba,htn,dm,cad,appet,pe,ane,class)) # �h�����O�ƭȫ��A��column
scale_outlier_data <- scale(outlier_data, center = TRUE, scale = TRUE)
scale_outlier_data <- as.data.frame(scale_outlier_data) 
head(scale_outlier_data)
for(i in 1:length(scale_outlier_data))
{
  scale_delete_outlier_data <- subset(scale_outlier_data, scale_outlier_data[,i] < 2)
}

for(i in 1:length(scale_delete_outlier_data))
{
  scale_delete_outlier_data <- subset(scale_delete_outlier_data, scale_outlier_data[,i] > -2)
}

#��Ž��
require(datasets) 
boxplot(scale_outlier_data)
boxplot(scale_delete_outlier_data) 

str(scale_delete_outlier_data)

#��Ƥ���
require(rpart)
set.seed(123)
train.index <- sample(x=1:nrow(dummy_knn_rm_Kidney_data), size=ceiling(0.8*nrow(dummy_knn_rm_Kidney_data) ))
train <- dummy_knn_rm_Kidney_data[train.index, ]
test <- dummy_knn_rm_Kidney_data[-train.index, ]



#ù�N��
str(train)
head(train)
model <- glm(cbind(classckd, classnotckd) ~., family = binomial(logit),train)
summary(model)
prob <- predict(model, newdata=test, type="response")
pred = rep("0", dim(test)[1])
pred[prob > 0.5] = "1"
table(pred, test$classckd)