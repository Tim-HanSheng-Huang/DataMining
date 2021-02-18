
marketing_data <- read.csv("C:/Users/user/Desktop/DM_Assignment_3_Marketing.csv")

head(marketing_data)

pca <- prcomp(formula = ~ Cloth+Home+House+Sport+Toys+Elect,  
              data = marketing_data,
              scale = TRUE) 
pca


plot(pca,         
     type="line", 
     main="Scree Plot for MarketingData") 
abline(h=1, col="blue") 


vars <- (pca$sdev)^2  
vars

props <- vars / sum(vars)    
props
cumulative.props <- cumsum(props)  
cumulative.props
cumulative.props[2]

plot(cumulative.props)

top2.pca.eigenvector <- pca$rotation[, 1:2]
top2.pca.eigenvector


first.pca <- top2.pca.eigenvector[, 1]   
second.pca <- top2.pca.eigenvector[, 2] 



# DOTCHART
first.pca[order(first.pca, decreasing=FALSE)]  
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   
         main="Loading Plot for PC1",                      
         xlab="Variable Loadings",                         
         col="red")                                        

second.pca[order(second.pca, decreasing=FALSE)]  
dotchart(second.pca[order(second.pca, decreasing=FALSE)] ,  
         main="Loading Plot for PC2",                       
         xlab="Variable Loadings",                          
         col="blue")                                        




