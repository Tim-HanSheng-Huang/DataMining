
marketing_data <- read.csv("C:/Users/user/Desktop/DM_Assignment_3_Marketing.csv")

str(marketing_data)

#線性迴歸的部分
model <- lm(formula= Elect ~ Cloth + Home + House + Sport + Toys,
            data=marketing_data)
summary(model)


# FORWARD
null = lm(Elect ~ 1, data = marketing_data)  
full = lm(Elect ~ ., data = marketing_data) 
forward.lm = step(null, 
                  scope=list(lower=null, upper=full), 
                  direction="forward")
summary(forward.lm)


# BACKWARD
full = lm(Elect ~ ., data = marketing_data)  
backward.lm = step(full, 
                   scope = list(upper=full), 
                   direction="backward")  
summary(backward.lm)


# BOTH
both_null=step(null, scope = list(upper=full), direction="both")
summary(both_null)
both_full=step(full, scope = list(upper=full), direction="both")
summary(both_full)

