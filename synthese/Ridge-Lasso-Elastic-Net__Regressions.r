library("tidyr")
library("purrr")
library("qgraph")
library("psych")
library("rgl")
library("ade4")
library("scatterplot3d")
library("ggplot2")
library("corrplot")
library("glasso")
library("FactoMineR")
library("factoextra")
library("tsne")
library("MASS")
library("dplyr")
library("tigerstats")
library("naniar")
library("MNM")
library("glmnet")

set.seed(42)

n <- 1000 # The made up dataset whill have n = 1000 samples
p <- 5000 # and p = 5000 parameters to estimate
real_p <- 15# however, only 15 of those parameters will help us predict the outcome.
# The remaining 4,985 parameters will just be random noise.
x <- matrix(rnorm(n*p),nrow= n, ncol = p)
y <- apply(x[,1:real_p],1,sum)+rnorm(n)

train_rows <- sample(1:n,.66*n)
x.train <- x[train_rows,]
x.test <- x[-train_rows,]

y.train <- y[train_rows]
y.test <- y[-train_rows]

# Ridge regression 
alpha0.fit <- cv.glmnet(x.train,y.train,type.measure = "mse", alpha = 0, family= "gaussian")
alpha0.predicted <- predict(alpha0.fit,s= alpha0.fit$lambda.1se,newx =x.test)
# s= alpha0.fit$lambda.1.se :
# s, which i think stands for "size" as in "the size of penalty" is set to one of the optimal values for lambda stored in alpha0.fit
mean((y.test - alpha0.predicted)^2)


#Lasso regression
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 1, familly= "gaussian")
alpha1.predicted <- predict(alpha1.fit, s= alpha1.fit$lambda.1se,newx = x.test)
mean((y.test - alpha1.predicted)^2)

# Elastic-Net regression
alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0.5, family="gaussian")
alpha0.5.predicted <- predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx =x.test)
mean((y.test - alpha0.5.predicted)^2)


list.of.list <- list()
for(i in 0:10){
  fit.name <- paste0("alpha", i/10)
  
  list.of.list[[fit.name]] <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha= i/10, family= "gaussian")
}

result <- data.frame()
  for(i in 0:10){
    fit.name <- paste0("alpha", i/10)
    predicted <- predict(list.of.list[[fit.name]],s= list.of.list[[fit.name]]$lambda.1se, newx = x.test)
    
    mse <- mean((y.test - predicted)^2)
    temp <- data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
    results <- rbind(results,temp)
    
  }
result














