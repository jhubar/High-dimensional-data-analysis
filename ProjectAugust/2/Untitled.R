setwd("/Users/julienhubar/Documents/#Master1/HDDA/High-dimensional-data-analysis/ProjectAugust/2")

#---------------------------------------------#
#                 Library                     #
#---------------------------------------------#
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
library("matlib")
library("ggpubr")



#---------------------------------------------#
#           Data pre-processing               #
#---------------------------------------------#

data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
attach(data)

#---------------------------------------------#
#     Strategy to treat missing data          #
#---------------------------------------------#
data <- data%>% dplyr::select ("V1","V2","V3","V4","V6","V7","V8","V11","V12","V13",
                               "V14","V15","V16","V17","V19","V20","V21","V22","V23","V27"
                               ,"V28","V29","V50","V24","V30","V31","V32","V33","V34",
                               "V35","V36","V37","V38","V39","V40","V41","V42","V43","V44","V45")

quali_Data <- data %>% dplyr::select("V1","V2","V3","V4","V6",
                                     "V7","V8","V11","V12","V13",
                                     "V14","V15","V16","V17","V19"
                                     ,"V20","V21","V22","V23","V27"
                                     ,"V28","V29","V50")
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}




#---------------------------------------------#
#       Robust outlier detection              #
#---------------------------------------------#

quanti_Data <- data[,c('V24','V30','V31','V32','V33','V34','V35','V36','V37','V38','V39','V40','V41','V42','V43','V44','V45')]
attach(quanti_Data)
Group <- data$V50
h = floor((dim(quanti_Data)[1]+ dim(quanti_Data)[2]+1)/2)
h-
robust <- cov.rob(quanti_Data, method="mcd", cor = TRUE, quantile.used = h, method = "mcd")
robEst <- cov.rob(quanti_Data, method="mcd", quantile.used=0.9*dim(quanti_Data)[2])
# Retrieve robust means and covariance matrix
robust_mean <- robust$center
robust_cov <- robust$cov
## Compute robust Mahalanobis distances
robust_maha <- mahalanobis(quanti_Data, robust_mean, robust_cov)
robust_maha <- data.frame(robust_maha)

pdf("DD-plot.pdf")
CMaha <- mahalanobis(quanti_Data, colMeans(quanti_Data), cov(quanti_Data))
RMaha <-mahalanobis(quanti_Data, robust_mean, robust_cov)
plot(CMaha,RMaha, col=V50+1,
     xlab="Squared classic Mahalanobis distance", ylab='Squared robust Mahalanobis distance', main="DD-plot")
abline(v=qchisq(0.95,dim(quanti_Data)[2]), col="blue") ; abline(h=qchisq(0.95,dim(quanti_Data)[2]), col="blue") ; legend("topleft",c("Die","live"), col=1:2, pch=16)
dev.off()


robEst <- cov.rob(quanti_Data, method="mcd", quantile.used=0.9*n)
dClass <- mahalanobis(quanti_Data, apply(quanti_Data, 2, mean), cov(quanti_Data))
dRob <- mahalanobis(quanti_Data , robEst$center, robEst$cov) 
plot(dClass,dRob, col=Group+1,
     xlab="Squared classic Mahalanobis distance", ylab='Squared robust Mahalanobis distance', main="DD-plot")
abline(v=qchisq(0.95,9), col="blue") ; abline(h=qchisq(0.95,9), col="blue") ; legend("topleft",c("die","live"), col=1:2, pch=16)