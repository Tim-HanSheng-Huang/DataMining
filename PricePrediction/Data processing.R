library(readr)
library(Hmisc)
library(ggplot2)

data<-data.frame(read.table("C:/Users/alice/Desktop/DM/project/DC_Properties_na.csv", header = TRUE, sep = ",", encoding = "UTF-8-BOM"))
length(data[,1])
data<-data[-which(is.na(data$X)),]
length(data[,1])

#impute missing value
data$AYB[which(is.na(data$AYB))]<-1900
describe(data$AYB)
data$STORIES[which(is.na(data$STORIES))]<-2
describe(data$STORIES)
describe(data)

#remove outlier
describe(data$PRICE)
hist(data$PRICE)
boxplot(data$PRICE)
qnt1 <- quantile(data$PRICE, probs=c(0, .99),na.rm = TRUE)
outlierCheck<-which(with(data,data$PRICE>=qnt1[2]))
outlierCheck
length(outlierCheck)#597

noOutliers <- data[-outlierCheck,]
data<-noOutliers
hist(data$PRICE)
length(data[,1])#56605

#Normalization the price
data$PRICE<-(data$PRICE-min(data$PRICE))/(max(data$PRICE)-min(data$PRICE))
#write.csv(data,"C:/Users/alice/Desktop/DM/project/DC_imputeNA_rmoutlier0.csv",fileEncoding = "big5")
write.csv(data,"C:/Users/alice/Desktop/DM/project/DC_imputeNA_rmoutlier.csv",fileEncoding = "big5")
#--------------------------------------------------------------------
#轉換dummy
require(dummies)
data <- read.csv('E:/DM/final/data/DC_final_20191229.csv',header=T, sep=",")
alldummy_data <- dummy.data.frame(data)
View(alldummy_data)
write.csv(alldummy_data, file = "E:/DM/final/data/DC_imputeNA_rmoutlier_dummy.csv", row.names=FALSE)