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

dataM <- data[V1=='1',]
quantDataM <- dataM %>% dplyr::select("V24","V30","V31","V32","V33",
                                      "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                                      "V45")

dataF <- data[V1=='0',]
quantDataF <- dataF %>% dplyr::select("V24","V30","V31","V32","V33",
                                      "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                                      "V45")

quantData <- data %>% dplyr::select("V24","V30","V31","V32","V33",
                               "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                               "V45")


#---------------------------------------------#
#       Robust outlier detection              #
#---------------------------------------------#


# Use MCD estimator

h <- floor( ((dim(quantData)[1] + dim(quantData)[2] + 1)/2))
hM<- floor( ((dim(quantDataM)[1] + dim(quantDataM)[2] + 1)/2))
hF<- floor( ((dim(quantDataF)[1] + dim(quantDataF)[2] + 1)/2))
set.seed(0)

robust <- cov.rob(quantData, cor = TRUE, quantile.used = h, method = "mcd")
robustM <- cov.rob(quantDataM, cor = TRUE, quantile.used = hM, method = "mcd")
robustF <- cov.rob(quantDataF, cor = TRUE, quantile.used = hF, method = "mcd")

# Retrieve robust means and covariance matrix
robust_mean <- robust$center
robust_cov <- robust$cov

robust_meanM <- robustM$center
robust_covM <- robustM$cov

robust_meanF <- robustF$center
robust_covF <- robustF$cov

## Compute robust Mahalanobis distances
robust_maha <- mahalanobis(quantData, robust_mean, robust_cov)
robust_maha <- data.frame(robust_maha)

robust_mahaM <- mahalanobis(quantDataM, robust_meanM, robust_covM)
robust_mahaM <- data.frame(robust_mahaM)

robust_mahaF <- mahalanobis(quantDataF, robust_meanF, robust_covF)
robust_mahaF <- data.frame(robust_maha)

## Compute sample average and sample covariance
sample_mean <- colMeans(quantData)
sample_cov <- cov(quantData)

sample_meanM <- colMeans(quantDataM)
sample_covM <- cov(quantDataM)

sample_meanF <- colMeans(quantDataF)
sample_covF <- cov(quantDataF)

## Compute classic Mahalanobis distances
classic_maha <- mahalanobis(quantData, sample_mean, sample_cov)
classic_maha <- data.frame(classic_maha)

classic_mahaM <- mahalanobis(quantDataM, sample_meanM, sample_covM)
classic_mahaM <- data.frame(classic_mahaM)

classic_mahaF <- mahalanobis(quantDataF, sample_meanF, sample_covF)
classic_mahaF <- data.frame(classic_mahaF)

## Compare both distances through a DD-plot
png("DD-Plot.png")
cmaha <-mahalanobis(quantData, sample_mean, sample_cov)
rmaha <-mahalanobis(quantData, robust_mean, robust_cov)
plot(cmaha,rmaha, col=V50+1,
     xlab="Squared classic Mahalanobis distance", ylab='Squared robust Mahalanobis distance', main="DD-plot")
abline(v=qchisq(0.95,dim(quantData)[2]), col="blue") ; abline(h=qchisq(0.95,dim(quantData)[2]), col="blue") ; legend("topleft",c("Die","Live"), col=1:2, pch=16)
dev.off()

pdf("DD-Plot_M_F.pdf")
par(mfrow=c(1,2))
cmahaM <-mahalanobis(quantDataM, sample_meanM, sample_covM)
rmahaM <-mahalanobis(quantDataM, robust_meanM, robust_covM)
plot(cmahaM,rmahaM, col=V50+1,
     xlab="Squared classic Mahalanobis distance", ylab='Squared robust Mahalanobis distance', main="DD-plot-Male")
abline(v=qchisq(0.95,dim(quantDataM)[2]), col="blue")
abline(h=qchisq(0.95,dim(quantDataM)[2]), col="blue") 
legend("topleft",c("Die","Live"), col=1:2, pch=16)

cmahaF <-mahalanobis(quantDataF, sample_meanF, sample_covF)
rmahaF <-mahalanobis(quantDataF, robust_meanF, robust_covF)
plot(cmahaF,rmahaF, col=V50+1,
     xlab="Squared classic Mahalanobis distance", ylab='Squared robust Mahalanobis distance', main="DD-plot-Female")
abline(v=qchisq(0.95,dim(quantDataF)[2]), col="blue") 
abline(h=qchisq(0.95,dim(quantDataF)[2]), col="blue")  
legend("topleft",c("Die","Live"), col=1:2, pch=16)
dev.off()


## Compute outlying rates for both methods
rob_outliers_rate <- sum(robust_maha > qchisq(.95, dim(quantData)[2]))/dim(data)[1]
classical_outliers_rate <- sum(classic_maha > qchisq(.95, dim(quantData)[2]))/dim(data)[1]

## Retrieve outlying observations for both methods
outlying_rob_idx <- robust_maha > qchisq(.95, dim(quantData)[2])
outlying_classic_idx <- classic_maha > qchisq(.95, dim(quantData)[2])

outlying_obs_rob <- quantData[outlying_rob_idx, ]
outlying_obs_classic <- quantData[outlying_classic_idx, ]

non_outlying <- anti_join(quantData, outlying_obs_rob)
common_outliers <- inner_join(outlying_obs_classic, outlying_obs_rob)
only_robust_outliers <- anti_join(outlying_obs_rob, outlying_obs_classic)

## Plot boxplots of quantitative data (outlying vs not outlying)
boxplot_data <- quantData
boxplot_data["rob_outlying"] = FALSE
boxplot_data[outlying_rob_idx, "rob_outlying"] = TRUE
library(reshape2)
melted <- melt(boxplot_data)
plt <- ggplot(melted, aes(x = rob_outlying, y = value)) + geom_boxplot()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
plt
ggsave("boxplots_outliers.pdf", plt)




#------------------------------------------------------------------#
# PART 3.2 : Further investigation of the correlation              #
#       structure of the quantitative variables                    #
#------------------------------------------------------------------#
library("qgraph")
## 1. Robust correlation matrix
h <- floor((dim(quantData)[1] + dim(quantData)[2] + 1) / 2)
robust <- cov.rob(quantData, cor = TRUE, quantile.used = h, method = "mcd")
robustM <- cov.rob(quantDataM, cor = TRUE, quantile.used = hM, method = "mcd")
robustF <- cov.rob(quantDataF, cor = TRUE, quantile.used = hF, method = "mcd")

classic_cor <- cor(quantData)
classic_corM <- cor(quantDataM)
classic_corF <- cor(quantDataF)
pdf("classic_correlation.pdf")
corrplot(classic_cor, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

pdf("robust_correlation.pdf")
corrplot(robust$cor, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

png("correlation_male_class.png")
par(mfrow=c(1,2))
corrplot(classic_corM, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
corrplot(robustM$cor, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()
png("correlation_female.png")
par(mfrow=c(1,2))
corrplot(classic_corF, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
corrplot(robustF$cor, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

### 2.a Classic covariance
classic_cov <- cov(quantData)
classic_covM <- cov(quantDataM)
classic_covF <- cov(quantDataF)
par(mfrow=c(3,2))
qgraph(solve(as.matrix(classic_cov)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3)
qgraph(solve(as.matrix(classic_covM)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3)
qgraph(solve(as.matrix(classic_covF)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3)
dev.off()
pdf("qgraph_Lambda0.pdf")
qgraph(glasso(cov(quantData), 0)$wi,edge.labels = T, directed = F, fade = F, diag = F, minimum = 1e-3)
dev.off()
pdf("qgraph.pdf")
par(mfrow=c(3,2))
qgraph(solve(as.matrix(classic_cov)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3)
qgraph(glasso(cov(quantData), 0)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantData), 0.005)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantData), 0.05)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantData), 0.5)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantData), 1)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
dev.off()


pdf("qgraph_LambdaM0.pdf")
qgraph(glasso(cov(quantData), 0)$wi,edge.labels = T, directed = F, fade = F, diag = F, minimum = 1e-3)
dev.off()
pdf("qgraphM.pdf")
par(mfrow=c(3,2))
qgraph(solve(as.matrix(classic_covM)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3)
qgraph(glasso(cov(quantDataM), 0)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataM), 0.005)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataM), 0.05)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataM), 0.5)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataM), 1)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
dev.off()

pdf("qgraphF.pdf")
par(mfrow=c(3,2))
qgraph(solve(as.matrix(classic_covF)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3)
qgraph(glasso(cov(quantDataF), 0)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataF), 0.005)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataF), 0.05)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataF), 0.5)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
qgraph(glasso(cov(quantDataF), 1)$wi, bidirectional = T, directed = F, fade = FALSE, diag = F, minimum = 1e-3)
dev.off()


res_PCA <- princomp(quantData, cor = TRUE)
res_PCAM <- princomp(quantDataM, cor = TRUE)
res_PCAF <- princomp(quantDataF, cor = TRUE)


# PCA circle

pdf("pcaCircleM.pdf")
fviz_pca_var(res_PCAM,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
dev.off()
par(mfrow=c(1,3))
pdf("scatterplotPCA.pdf")
plot(res_PCA$scores[,1], res_PCA$scores[,2], col=V50+1, xlab="PC1", ylab="PC2", main = "scatter plot PCA")
dev.off()
plot(res_PCAM$scores[,1], res_PCAM$scores[,2], col=V50+1, xlab="PC1", ylab="PC2")
plot(res_PCAF$scores[,1], res_PCAF$scores[,2], col=V50+1, xlab="PC1", ylab="PC2")
pdf("pcaCircleF.pdf")
fviz_pca_var(res_PCAF,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
dev.off()

summary(res_PCAM)
summary(res_PCAF)

pdf("screeplot.pdf")
par(mfrow=c(1,2))
plot(res_PCAM, type='l',main="scree plot men")
plot(res_PCAF, type='l',main="scree plot women")
dev.off()


##################################################
# ---------- PART 3.3.2 : tSNE Analysis ----------
##################################################

#ALL
# 1. 2D-projection
library(Rtsne)

restSNE2 <- Rtsne(X=quantData, perplexity=2)
restSNE5 <- Rtsne(X=quantData, perplexity=5)
restSNE10 <- Rtsne(X=quantData, perplexity=10)
restSNE20 <- Rtsne(X=quantData, perplexity=20)
restSNE30 <- Rtsne(X=quantData, perplexity=30)
restSNE50 <- Rtsne(X=quantData, perplexity=50)

pdf("tsneMF.pdf")
par(mfrow=c(2,3))
plot(restSNE2$Y, col=V1+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=2")
legend("topleft",c("Female","Male"), col=1:2, pch=16)
plot(restSNE5$Y, col=V1+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=5")
legend("bottomleft",c("Female","Male"), col=1:2, pch=16)
plot(restSNE10$Y, col=V1+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=10")
legend("topright",c("Female","Male"), col=1:2, pch=16)
plot(restSNE20$Y, col=V1+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=20")
legend("bottomleft",c("Female","Male"), col=1:2, pch=16)
plot(restSNE30$Y, col=V1+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=30")
legend("topleft",c("Female","Male"), col=1:2, pch=16)
plot(restSNE50$Y, col=V1+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=50")
legend("topleft",c("Female","Male"), col=1:2, pch=16)
dev.off()

#MEN

restSNE2M <- Rtsne(X=quantDataM, perplexity=2)
restSNE5M <- Rtsne(X=quantDataM, perplexity=5)
restSNE10M <- Rtsne(X=quantDataM, perplexity=10)
restSNE20M <- Rtsne(X=quantDataM, perplexity=20)
restSNE30M <- Rtsne(X=quantDataM, perplexity=30)
restSNE40M <- Rtsne(X=quantDataM, perplexity=40)


#pdf("tsneMale.pdf")
par(mfrow=c(2,3))
plot(restSNE2M$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=2")
plot(restSNE5M$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=5")
plot(restSNE10M$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=10")
plot(restSNE20M$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=20")
plot(restSNE30M$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=30")
plot(restSNE40M$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=40")
#dev.off()


restSNE2F <- Rtsne(X=quantDataF, perplexity=2)
restSNE5F <- Rtsne(X=quantDataF, perplexity=5)
restSNE10F <- Rtsne(X=quantDataF, perplexity=10)


#pdf("tsne.pdf")
par(mfrow=c(1,3))
plot(restSNE2F$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=2")
plot(restSNE5F$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=5")
plot(restSNE10F$Y, col=V50+1, xlab="tSNE1", ylab="tSNE2", main="Perplexity=10")
#dev.off()

