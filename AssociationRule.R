
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)

groceries_data = read.transactions("C:/Users/user/Desktop/DM_Assignment_2_groceries_2.csv",sep=",")

head(groceries_data)

summary(groceries_data)

#ºâ¥XÀW²v¹Ï
itemFrequencyPlot(groceries_data,topN=20,type="absolute")

rule1 = apriori(groceries_data,parameter = list(support = 0.001,confidence = 0.15))
summary(rule1)
sort.rule1 <- sort(rule1, by="lift")
inspect(sort.rule1[1:5])

require(arulesViz)

plot(sort.rule1,method="graph",interactive=TRUE,shading=NA)
plot(sort.rule1[1:50])
plot(sort.rule1[1:50],method="graph")
plot(sort.rule1[1:50], method="grouped")
