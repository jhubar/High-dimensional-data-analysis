setwd("/Users/julienhubar/Documents/#Master1/HDDA/High-dimensional-data-analysis/ProjectAugust/2")

#---------------------------------------------#
#                 Library                     #
#---------------------------------------------#


library(dplyr)
library(reshape2)
library(corrplot)
library(reshape2)
library(corrplot)
library(MASS)


#---------------------------------------------#
#           Data pre-processing               #
#---------------------------------------------#

data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
attach(data)

#---------------------------------------------#
#     Strategy to treat missing data          #
#---------------------------------------------#
data <- data%>% select ("V1","V2","V3","V4","V6","V7","V8","V11","V12","V13",
                        "V14","V15","V16","V17","V19","V20","V21","V22","V23","V27"
                        ,"V28","V29","V50","V24","V30","V31","V32","V33","V34",
                        "V35","V36","V37","V38","V39","V40","V41","V42","V43","V44","V45")

quali_Data <- data %>% select("V1","V2","V3","V4","V6",
                              "V7","V8","V11","V12","V13",
                              "V14","V15","V16","V17","V19"
                              ,"V20","V21","V22","V23","V27"
                              ,"V28","V29","V50")
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}



quanti_Data <- data %>% select("V24","V30","V31","V32","V33",
                               "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                               "V45")


#---------------------------------------------#
#       Robust outlier detection              #
#---------------------------------------------#


# Use MCD estimator (Coverage parameter of 0.75)

h <- floor((dim(quanti_Data)[1] + dim(quanti_Data)[2] + 1)/2)
set.seed(0)
robust <- cov.rob(quanti_Data, cor = TRUE, quantile.used = h, method = "mcd")

# Retrieve robust means and covariance matrix
robust_mean <- robust$center
robust_cov <- robust$cov

## Compute robust Mahalanobis distances
robust_maha <- mahalanobis(quanti_Data, robust_mean, robust_cov)
robust_maha <- data.frame(robust_maha)

## Compute sample average and sample covariance
sample_mean <- colMeans(quanti_Data)
sample_cov <- cov(quanti_Data)

## Compute classic Mahalanobis distances
classic_maha <- mahalanobis(quanti_Data, sample_mean, sample_cov)
classic_maha <- data.frame(classic_maha)

## Compare both distances through a DD-plot
distances <- data.frame(c(classic_maha, robust_maha))
plt <- ggplot(distances, aes(x = distances[, 1], y = distances[, 2]))
plt <- plt + geom_point() + geom_vline(xintercept = qchisq(0.95, dim(quanti_Data)[2]), col = "red") 
plt <- plt + geom_hline(yintercept = qchisq(0.95, dim(quanti_Data)[2]), col = "red") 
plt <- plt + labs(x = "Classic Mahalanobis distances", y = "Robust Mahalanobis distances")
ggsave(filename = "compare_maha_most_robust.pdf")

## Compute outlying rates for both methods
rob_outliers_rate <- sum(robust_maha > qchisq(.95, dim(quanti_Data)[2]))/dim(data)[1]
classical_outliers_rate <- sum(classic_maha > qchisq(.95, dim(quanti_Data)[2]))/dim(data)[1]

## Retrieve outlying observations for both methods
outlying_rob_idx <- robust_maha > qchisq(.95, dim(quanti_Data)[2])
outlying_classic_idx <- classic_maha > qchisq(.95, dim(quanti_Data)[2])

outlying_obs_rob <- quanti_Data[outlying_rob_idx, ]
outlying_obs_classic <- quanti_Data[outlying_classic_idx, ] 

non_outlying <- anti_join(quanti_Data, outlying_obs_rob)
common_outliers <- inner_join(outlying_obs_classic, outlying_obs_rob)
only_robust_outliers <- anti_join(outlying_obs_rob, outlying_obs_classic)

## Plot boxplots of quantitative data (outlying vs not outlying)
boxplot_data <- quanti_Data
boxplot_data["rob_outlying"] = FALSE
boxplot_data[outlying_rob_idx, "rob_outlying"] = TRUE

melted <- melt(boxplot_data)
plt <- ggplot(melted, aes(x = rob_outlying, y = value)) + geom_boxplot()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("boxplots_outliers.pdf", plt) 

## Plot histograms of quantitative data (non outlying)
melted <- melt(non_outlying)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("histograms_non_outlying.pdf", plt) 


## Plot histograms of common outlying observations
melted <- melt(common_outliers)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("histograms_common_outliers.pdf", plt)

## Plot histograms of robust outlying observations not detected by the classical approach
melted <- melt(only_robust_outliers)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("histograms_robust_only.pdf", plt)

## Plot histograms of all robust outlying observations
melted <- melt(outlying_obs_rob)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("histograms_robust_all.pdf", plt) 


#------------------------------------------------------------------# 
# PART 3.2 : Further investigation of the correlation              #
#       structure of the quantitative variables                    #
#------------------------------------------------------------------# 

## 1. Robust correlation matrix
h <- floor((dim(quanti_Data)[1] + dim(quanti_Data)[2] + 1) / 2)
robust <- cov.rob(quanti_Data, cor = TRUE, quantile.used = h, method = "mcd")

classic_cor <- cor(quanti_Data)
pdf("classic_correlation.pdf")
corrplot(classic_cor, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

pdf("robust_correlation.pdf")
corrplot(robust$cor, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

library(qgraph)
### 2.a Classic covariance
classic_cov <- cov(quanti_Data)
qgraph(solve(as.matrix(classic_cov)), fade = FALSE, edge.labels = TRUE, 
       diag = FALSE, minimum = 1e-3, filetype = "pdf", filename = "qgraph_classic_cov")

### 2.b L1-regularized covariance

#### Optimal lambda using BIC method
lambda <- seq(0, 1, length.out = 10)
BIC <- rep(0, 10)

n <- dim(quantitative_data)[1]
p <- dim(quantitative_data)[2]

for (i in 1:length(lambda)) {
  l1reg <- huge(classic_cov, lambda[i], method = "glasso", cov.output = TRUE)
  l1prec <- l1reg$icov[[1]]
  BIC[i] <- -n * l1reg$loglik +
    log(n) * (p + sum(l1prec[upper.tri(l1prec, diag = TRUE)] != 0))
}

plt <- ggplot() + geom_line(aes(x = lambda, y = BIC))
ggsave(filename = "products/pdf/optimal_lambda.pdf", plt)

lambda <- lambda[which.min(BIC)]

#### L1-regularization with optimal lambda
l1reg <- huge(classic_cov, lambda, method = "glasso", cov.output = TRUE)
l1_cov <- l1reg$cov[[1]]

rownames(l1_cov) <- rownames(classic_cov)
colnames(l1_cov) <- colnames(classic_cov)

qgraph(solve(as.matrix(l1_cov)), fade = FALSE, edge.labels = TRUE, diag = FALSE, minimum = 1e-3, filetype = "pdf", filename = "products/pdf/qgraph_l1_cov")

