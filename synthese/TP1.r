library("dplyr")
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
library("tigerstats")
library("naniar")
library("MNM")
setwd("/Users/Julien/Documents/#Master1/HDAD/cours")
data <- read.table("/Users/Julien/Documents/#Master1/HDAD/cours/car.txt", sep=",", header = TRUE)
attach(data)



x <- data[c(10:14,17,19:26)]

mcor1<-cor(x,use="complete.obs")
mcor2 <-cor(x,use = "pairwise.complete.obs")
par(mfrow = c(1,2))
corrplot(mcor1)
corrplot(mcor2)

tr <- sum(diag(var(x,na.rm = TRUE)))
diag(var(x,na.rm =TRUE))/tr

#We see that the variable Price accounts for 99% of
# the total variability! Clearly, standardisation will
# be required as soon as techniques depending on marginal variation will be used.


#exercice2

colvector <- as.integer(Price<median(Price,na.rm= TRUE))+1
plot3d(x[,1:3],col=colvector)

#exercie3

library(TeachingDemos)
stars(x)


#Ex 4

m<-colMeans(x,na.rm=TRUE)
S<-var(x,na.rm=TRUE)
d<-mahalanobis(x,m,S)
plot(d,type="h")
abline(h=qchisq(0.975,14))



#Pay attention to the missing values and  to the potential "violation" of the normality assumption (quite expected when looking at pairsplots

#Comparison with 14 univeriate testing at the 5% level


detectp<- as.integer(d >qchisq(0.975,14) )
# binary indicator for the multivariate detection
# The matrix res contains the binary indicators corresponding to each of the 14 possible 1D outlier detection

res<- NULL
for (j in 1:14)
{
  mj<-mean(x[,j],na.rm=TRUE)
  sj<-sd(x[,j],na.rm=TRUE)
  detectj<-as.integer(abs(x[,j]-mj)/sj > 2)
  res<-cbind(res,detectj)
}

#The table below compare the unique multivariate outlier binary indicator with the number of univariate analyses which lead to a 1D outlier detection

table(detectp,apply(res,1,sum))
#8% of observations detected as outliers in p dim
#30% of observations are detected at least once when using 14 1D detections

# The results obtained with a univariate cutoff given by 2.69 (=qnorm(0.95/p)) instead of 2 are a bit better!


#1.2
#More "sophisticated" techniques may be used to determine the numbers of missing values for each variable (see Practical 1) but these are also available in the output of summary

#1.3
#let us choose the variable Price
#In order not to modify the original Price variable, the two imputation examples will be performed via another object of R
var1<-Price

#The initial 95% (asymptotic) confidence interval for the true mean of Price is given by mean +/- qnorm(0.975) * s/sqrt(n) where n is the number of non-missing observations and qnorm(0.975) is the 97.5 quantile of the standard normal distribution 
mean(Price,na.rm=TRUE)-qnorm(0.975)*sd(Price,na.rm=TRUE)/sqrt(length(Price)-4)
mean(Price,na.rm=TRUE)+qnorm(0.975)*sd(Price,na.rm=TRUE)/sqrt(length(Price)-4)
#Imputation of the missing data by means of the average of the non-missing observations
m<-mean(var1,na.rm=TRUE)
var1[is.na(var1)]<-m

#New CI
mean(var1)-qnorm(0.975)*sd(var1)/sqrt(length(var1))
mean(var1)+qnorm(0.975)*sd(var1)/sqrt(length(var1))
#The CI is a bit narrower, as expected

#Conditional imputation of the missing data by means of a linear regression line
var2<-Price
cor(Price,data[,c(10:14, 17, 19:26)],use="complete.obs")
corrplot(cor(Price,data[,c(10:14, 17, 19:26)],use="complete.obs"))
#Price is highly correlated with EngineSize (with a somewhat visible linear trend on the scatter plot), and that variable does not count any missing values. A linear model of Price wrt EngineSize may then be fitted and the missing values are imputed by the corresponding fitted values.

mod<-lm(Price~EngineSize)$coefficients


lm.price<-lm(Price~EngineSize)
plot(lm.price)
var2[is.na(var2)]<-mod[1]+mod[2]*EngineSize[is.na(var2)==TRUE]

#New CI
mean(var2)-qnorm(0.975)*sd(var2)/sqrt(length(var2))
mean(var2)+qnorm(0.975)*sd(var2)/sqrt(length(var2))
#The CI is a bit narrower, as expected








