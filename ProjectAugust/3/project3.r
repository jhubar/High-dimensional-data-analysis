setwd("/Users/julienhubar/Documents/#Master1/HDDA/High-dimensional-data-analysis/ProjectAugust/3")

#---------------------------------------------#
#                 Library                     #
#---------------------------------------------#

library(ggplot2)
library(dplyr)
library(lubridate)
library(plotrix)
library(naniar)
library(plotrix)
library(colorspace)
library(visdat)
library(reshape2)
library(corrplot)
library(tidyverse)
library(reshape2)
library(car)
library(rgl)
library(MASS)
library(hexbin)
library(readtext)

#---------------------------------------------#
#           Data pre-processing               #
#---------------------------------------------#

data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
attach(data)

#---------------------------------------------#
#     Strategy to treat missing data          #
#---------------------------------------------#
data <- data%>% dplyr::select ("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                        "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                        "V27","V28","V29","V50","V24","V30","V31","V32","V33",
                        "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                        "V45")
quali_Data <- data %>% dplyr::select("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                              "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                              "V27","V28","V29","V50")
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}



quanti_Data <- data %>% dplyr::select("V24","V30","V31","V32","V33",
                               "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                               "V45")

quanti_var <- c('V24','V30','V31','V32','V33',
                'V34','V35','V36','V37','V38','V39','V40','V41','V42','V43','V44',
                'V45')
#-----------------------------------------------------# 
#           Linear Discriminant Analysis              #
#-----------------------------------------------------# 



quanti_Data <- scale(quanti_Data) # scaled !

g <- 2
n <- dim(data)[1]

lda_full <- lda(x=quanti_Data, grouping =V50 )

## Canonical variable
print(lda_full$scaling)

## Scores
scores <- as.matrix(quanti_Data) %*% cbind(lda_full$scaling)

pdf("lda_full_scatter.pdf")
plot(scores, V50)
dev.off()

pdf("lda_full_boxplot.pdf")
boxplot(scores ~ V50)
dev.off()

## Power
l1 <- (g - 1) * lda_full$svd[1]^2 / n
gamma1 <- l1 / (1 + l1)
print(gamma1)

# 2. Leave one out
p <- length(quanti_var)

gamma = rep(0, p)

for (i in 1:p) {
  index <- rep(TRUE, p)
  index[i] = FALSE
  
  lda_partial <- lda(x=quanti_Data[, index], grouping=alert)
  l <- (g - 1) * lda_partial$svd[1]^2 / n
  gamma[i] <- l / (1 + l)
}

cbind(quanti_var, gamma)

quanti_var <- c('month', 'temp', 'pres', 'dewp', 'wspd', 'wdir')
quantitative_data <- scale(data[, quantitative_var]) # scaled !

lda_final <- lda(x=quantitative_data, grouping=alert)

## Canonical variable
print(lda_final$scaling)

## Power
l <- (g - 1) * lda_final$svd[1]^2 / n
gamma <- l / (1 + l)
print(gamma)

## Scores
scores <- as.matrix(quantitative_data) %*% cbind(lda_final$scaling)

# 3. Classification

## Prior
table(alert) / length(alert)

## Leave-one-out

pred <- rep(0, n)

for (i in 1:n) {
  index <- rep(TRUE, n)
  index[i] <- FALSE
  
  lda <- lda(x=quantitative_data[index,], grouping=alert[index])
  
  scores <- as.matrix(quantitative_data[index,]) %*% cbind(lda$scaling)
  
  g0 <- scores[alert[index] == 0]
  g1 <- scores[alert[index] == 1]
  
  mu0 <- mean(g0)
  mu1 <- mean(g1)
  
  z <- sum(quantitative_data[i,] * cbind(lda$scaling))
  
  pred[i] <- abs(z - mu1) < abs(z - mu0)
}

## Confusion matrix
conf_mat <- table(alert, pred)

print(conf_mat)

# 4. Homoscedasticity

cov <- cov(quantitative_data[alert == 0,])
pdf("cov_group_false.pdf")
corrplot(cov, is.corr=FALSE, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

cov <- cov(quantitative_data[alert == 1,])
pdf("cov_group_true.pdf")
corrplot(cov, is.corr=FALSE, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

var(g0)
var(g1) 