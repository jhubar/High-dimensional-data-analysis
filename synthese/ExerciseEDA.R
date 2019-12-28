#Exercises on univariate EDA

#1.1

data<-read.table("car.txt",header=TRUE,sep=",")
attach(data)
#If the data are downloaded directly from the web site (and not from eCampus), it is necessary to pay attention to the notation for the missing values!

# In order to check the variable types, use the summary command
summary(data)

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
#Price is highly correlated with EngineSize (with a somewhat visible linear trend on the scatter plot), and that variable does not count any missing values. A linear model of Price wrt EngineSize may then be fitted and the missing values are imputed by the corresponding fitted values.

mod<-lm(Price~EngineSize)$coefficients
var2[is.na(var2)]<-mod[1]+mod[2]*EngineSize[is.na(var2)==TRUE]

#New CI
mean(var2)-qnorm(0.975)*sd(var2)/sqrt(length(var2))
mean(var2)+qnorm(0.975)*sd(var2)/sqrt(length(var2))
#The CI is a bit narrower, as expected

#NB: a more complex model - with more than one explanatory variable  - might have been fitted.

#2.1

#let us focus only on the continuous variables 
#looking at the output of summary, we may detect the variables Price as well as (to a less extent) the variables Loss and Horsepower as having a deviation between the mean and medina (NB: these deviations need to be considered with respect to the overall dispersion of the variable)

hist(Price)

# Clearly the distribution is highly skewed.
# Therefore,n it might be a good idea to use a boxplot adjusted to the underlying skewness.

library(robustbase)
adjbox(Price)

#NB: when symmetry is a condition that need to be fulfilled, it may sometimes be useful to transform the data by means of a square root or a logarithm
hist(log(Price))
adjbox(log(Price))
#In that case, the adjustment of the boxplot is not really necessary.


#2.2
# In order to determine whether the normality assumption holds, we may look at the histogram and the boxplot;
# A deviation from symmetry (not only due to some specific outliers) is an indication of violation of normality; also a multi-modal histogram (several peaks) is a violation of the normality
hist(Height)
boxplot(Height)
hist(Width)
boxplot(Width)
# in both cases, the boxplot does not show any worrying signs.
# in the case of length, the unimodality of the histogram is questionnable, while the skewness of the histogram of width is also quite pronounced.
 

#We may confirm these statements with a normality test (in both cases, the null hypothesis of normality is rejected)
library(nortest)
lillie.test(Height)
lillie.test(Width)

#2.3
#Let us focus on Compression Ratio

zscore<-scale(CompressionRatio)
plot(zscore,type="h")


truncCompr<-sort(CompressionRatio)[(floor(205*0.1)+1):(205-floor(205*0.1))]
zscorerob<-(CompressionRatio-mean(truncCompr))/sd(truncCompr)

#It is possible to compute a trimmed mean by means of the function mean: mean(CompressionRatio,trim=0.1)

plot(zscorerob,type="h")


plot(zscore,zscorerob)
#We see that the robust detection procedure pinpoints more observations than the classical one. Nevertheless, the main "outlying" cars are detected by both techniques and these are 20 diesel cars! 
#two additional (gas) cars have robust zscores which are still quite big: cars n°10 (an audi) and n°50 (a jaguar)

#2.4
boxplot(Price~Fuel, varwidth=TRUE)
plot(ecdf(Price[Fuel=="gaz"]))

plot(ecdf(Price[Fuel=="gas"]),main="")
lines(ecdf(Price[Fuel=="diesel"]),col="red")

#These plots were already discussed during the lecture.

#Exercises on multivariate EDA


#Ex 1
x<-data[,c(10:14, 17, 19:26)]

cor1<-cor(x,use="complete.obs")
cor2<-cor(x,use="pairwise.complete.obs")

#No visible difference between the two options; this is not surprising as the percentage of missing data (when deleting the variable Loss) is quite limited.

library(corrplot)
corrplot(cor1)

tr<-sum(diag(var(x,na.rm=TRUE)))
diag(var(x,na.rm=TRUE))/tr
#We see that the variable Price accounts for 99% of the total variability! Clearly, standardisation will be required as soon as techniques depending on marginal variation will be used.

#Ex 2

library(rgl)
colvector<-as.integer(Price<median(Price,na.rm=TRUE))+1
plot3d(x[,1:3],col=colvector)

#We can indeed see that the two sets of points are not completely mixed. It should be possible to "guess" the fact that the price is below or above median by means of the three variables

#Ex 3

library(TeachingDemos)
stars(x)

# It is quite subjective to determine a number of clusters using that picture, but still it might provide a good starting value for techniques depending on a pre-defined number of groups.

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













