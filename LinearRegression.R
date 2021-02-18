
winequality <- read.csv("C:/Users/user/Desktop/DM_Assignment1_winequality.csv")
head(winequality)
str(winequality)


colnames(winequality)[colnames(winequality)=="﻿fixed.acidity"] <- "fixed.acidity"
colnames(winequality)

#regression analysis
lm.model <- lm(formula= quality ~ 
                 fixed.acidity+
                 volatile.acidity+
                 citric.acid+
                 residual.sugar+
                 chlorides+
                 free.sulfur.dioxide+
                 total.sulfur.dioxide+
                 total.sulfur.dioxide+
                 density+
                 pH+
                 sulphates+
                 alcohol,
               data=winequality)
summary(lm.model)


#Normality
shapiro.test(lm.model$residual)

#Independence
require(car)
durbinWatsonTest(lm.model) 

#Homogeneity of Variance with respect to residual
ncvTest(lm.model)

