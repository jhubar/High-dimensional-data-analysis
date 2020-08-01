setwd("/Users/julienhubar/Documents/#Master1/HDDA/High-dimensional-data-analysis/ProjectAugust")
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
library(gridExtra)
#---------------------------------------------#
#           Data pre-processing               #
#---------------------------------------------#
data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
attach(data)


quali_Data <- data %>% select("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12",
                              "V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V27","V28","V29","V50")
quanti_Data <- data %>% select("V24","V25","V26","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39"
                               ,"V40","V41","V42","V43","V44","V45","V46","V47","V48","V49")


#---------------------------------------#


pdf("hist_.pdf")
par(mfrow=c(5,5))
hist(V1)
hist(V2)
hist(V3)
hist(V4)
hist(V6)
hist(V7)
hist(V8)
hist(V11)
hist(V12)
hist(V13)
hist(V14)
hist(V15)
hist(V16)
hist(V17)
hist(V19)
hist(V20)
hist(V21)
hist(V22)
hist(V23)
hist(V27)
hist(V28)
hist(V29)
hist(V50)
dev.off()

#---------------------------------------#
pdf("hist1-6.pdf")
par(mfrow=c(3,2))
hist(V1)
hist(V2)
hist(V3)
hist(V4)
hist(V5)
hist(V6)
dev.off()

pdf("hist7-12.pdf")
par(mfrow=c(3,2))
hist(V7)
hist(V8)
hist(V9)
hist(V10)
hist(V11)
hist(V12)
dev.off()

pdf("hist13-18.pdf")
par(mfrow=c(3,2))
hist(V13)
hist(V14)
hist(V15)
hist(V16)
hist(V17)
hist(V18)
dev.off()

pdf("hist19-23.pdf")
par(mfrow=c(3,2))
hist(V19)
hist(V20)
hist(V21)
hist(V22)
hist(V23)
dev.off()

pdf("hist27-29.pdf")
par(mfrow=c(3,1))
hist(V27)
hist(V28)
hist(V29)
dev.off()


#---------------------------------------------#
#             Part 2: Missingness             #
#---------------------------------------------#

# Missingness visualisation quali
ggsave("MissingnessVisualisation_Quali.png")
vis_miss(quali_Data, sort_miss = TRUE)
dev.off()


# Missingness visualisation quanti
ggsave("MissingnessVisualisation_Quanti.png")
vis_miss(quanti_Data, sort_miss = TRUE)
dev.off()

# Deletion of columns with more than 20% missing data

data <- data%>% select ("V1","V2","V3","V4","V6","V7","V8","V11","V12",
         "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
         "V27","V28","V29","V50","V24","V30","V31","V32","V33",
         "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
         "V45")

quali_Data <- data %>% select("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                              "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                              "V27","V28","V29","V50")

quanti_Data <- data %>% select("V24","V30","V31","V32","V33",
                               "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                               "V45")


# Missingness visualisation quali after deletion
pdf("MissingnessVisualisation_Quali_after_deletion.pdf")
vis_miss(quali_Data, sort_miss = TRUE)
dev.off()


# Missingness visualisation quanti after deletion
pdf("MissingnessVisualisation_Quanti_after_deletion.pdf")
vis_miss(quanti_Data, sort_miss = TRUE)
dev.off()

## Missingness 
columns <- colnames(data)
columns_miss <- columns[colSums(is.na(data)) > 0]
pdf("missingness_patterns.pdf")
gg_miss_upset(data, nset = length(columns_miss))
dev.off()

# Missingness rate for each columns
pdf("missingness_rates.pdf")
nb_na <- colSums(is.na(data[, columns_miss]))
barplot(nb_na / nrow(data), legend.text = nb_na, col = rainbow_hcl(length(columns_miss)))
dev.off()

## Z(i, j) = z-score of mean(i | is.na(j)) as an estimator of mean(i)
Z <- matrix(NA, length(columns), length(columns_miss))
rownames(Z) <- columns
colnames(Z) <- columns_miss
means <- apply(data, 2, function (x) mean(x, na.rm = TRUE))
stds <- apply(data, 2, function (x) sd(x, na.rm = TRUE))
for (i in 1:length(columns_miss)) {
  indexes_NA <- is.na(data[columns_miss[i]])
  means_NA <- apply(data[indexes_NA, ], 2, function (x) mean(x, na.rm = TRUE))
  n_NA <- sum(indexes_NA)
  Z[, i] <- (means_NA - means) / (stds / sqrt(n_NA))
}

#---------------------------------------------#
#  Part 2.1: Strategy to treat missing data   #
#---------------------------------------------#

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#---------------------------------------------#
#      Part 3: Explanatory analysis           #
#---------------------------------------------#
quali_Data <- data %>% select("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                              "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                              "V27","V28","V29","V50")

quanti_Data <- data %>% select("V24","V30","V31","V32","V33",
                               "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                               "V45")

#---------------------------------------------#
#  Part 3.1: Univariate exploratory analysis  #
#---------------------------------------------#

melted <- melt(quanti_Data)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("histograms_quanti.pdf", plt)

melted <- melt(quali_Data)
plt <- ggplot(melted, aes(x = value)) + geom_histogram()
plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
ggsave("histograms_quali.pdf", plt)


#---------------------------------------------#
# Part 3.2: Multivariate exploratory analysis #
#---------------------------------------------#

# correlation between varibles
corr <- cor(data[,24:30])
pdf("correlation.pdf")
corrplot(corr, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

# correlation btw quali varibles
corr_quali <- cor(quali_Data)
pdf("correlation_quali.pdf")
corrplot(corr_quali, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

# correlation btw quali varibles
corr_quanti <- cor(quanti_Data)
pdf("correlation_quanti.pdf")
corrplot(corr_quanti, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()

specie <- c(rep("V1" , 2) , rep("V2" , 2) , rep("V3" , 2) , rep("V4" , 2),
            rep("V6" , 2) , rep("V7" , 2) , rep("V8" , 2) , rep("V11" , 2),
            rep("V12" , 2) , rep("V13" , 2) , rep("V14" , 2) , rep("V15" , 2),
            rep("V16" , 2) , rep("V17" , 2) , rep("V19" , 2) , rep("V20" , 2),
            rep("V21" , 2) , rep("V22" , 2) , rep("V23" , 2) , rep("V27" , 6),
            rep("V28" , 3) , rep("V29" , 3) , rep("V50" , 2) 
            )
condition <- rep(c("0" , "1" , "2", "3", "4","5") , 23)
value <- abs(rnorm(12 , 0 , 15))
test_data <- data.frame(specie,condition,value)
boxplot(data$V1)

# Stacked
ggplot(test_data, aes(fill = condition, y= value, x=specie)) + 
  geom_bar(position="stack", stat="identity")
#---------------------------------------------#
# PART 3.3 : Qualitative variables impact on  #
#            quantitative one                 #
#---------------------------------------------#
quali_cols <- c("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                "V27","V28","V29","V50")

quanti_cols <- c("V24","V30","V31","V32","V33",
                 "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                 "V45")

# /!\ IL est interressant de voir que la plupart des patients atteint d'une ciroses meurt du HCC

pdf("tableV1V3.pdf", width=1, height=1)
df <- table(V1,V3)
colnames(df) <- c("V1")
rownames(df) <- c("V3")
df <- as.table(df)
grid.table(df)
dev.off()
pdf("tablev4V50.pdf", width=1, height=1)
table(V4,V50)
dev.off()
pdf("tableV6V50.pdf", width=1, height=1)
table(V6,V50)
dev.off()
pdf("tableV7V50.pdf", width=1, height=1)
grid.table(table(V7,V50))
dev.off()
pdf("tableV8V50.pdf", width=1, height=1)
grid.table(table(V8,V50))
dev.off()
pdf("tableV1V8.pdf", width=1, height=1)
table(V1,V8)
dev.off()
pdf("tableV3V50.pdf", width=1, height=1)
table(V3,V50) 
dev.off()





data_test <- data %>% select("V30","V32",
                             "V36","V38","V39","V40","V41")
pdf("pairs_qunati_Data.pdf")
pairs(data_test)
dev.off()
# Impact of gender
pdf("boxplot1V1.pdf")
par(mfrow=c(2,4))
boxplot(V24~V1)
boxplot(V30~V1)
boxplot(V31~V1)
boxplot(V32~V1)
boxplot(V33~V1)
boxplot(V34~V1)
boxplot(V35~V1)
boxplot(V36~V1)
boxplot(V37~V1)
boxplot(V38~V1)
boxplot(V39~V1)
boxplot(V40~V1)
boxplot(V41~V1)
boxplot(V42~V1)
boxplot(V43~V1)
boxplot(V44~V1)
boxplot(V45~V1)
dev.off()

# Impact of Alcohol
pdf("boxplot1V3.pdf")
par(mfrow=c(2,4))
boxplot(V24~V3)
boxplot(V30~V3)
boxplot(V31~V3)
boxplot(V32~V3)
boxplot(V33~V3)
boxplot(V34~V3)
boxplot(V35~V3)
boxplot(V36~V3)
boxplot(V37~V3)
boxplot(V38~V3)
boxplot(V39~V3)
boxplot(V40~V3)
boxplot(V41~V3)
boxplot(V42~V3)
boxplot(V43~V3)
boxplot(V44~V3)
boxplot(V45~V3)
dev.off()

# Impact of
pdf("boxplot1V4.pdf")
par(mfrow=c(2,4))
boxplot(V24~V4)
boxplot(V30~V4)
boxplot(V31~V4)
boxplot(V32~V4)
boxplot(V33~V4)
boxplot(V34~V4)
boxplot(V35~V4)
boxplot(V36~V4)
boxplot(V37~V4)
boxplot(V38~V4)
boxplot(V39~V4)
boxplot(V40~V4)
boxplot(V41~V4)
boxplot(V42~V4)
boxplot(V43~V4)
boxplot(V44~V4)
boxplot(V45~V4)
dev.off()
# Impact of Diabetes 
pdf("boxplot1V11.pdf")
par(mfrow=c(2,4))
boxplot(V24~V11)
boxplot(V30~V11)
boxplot(V31~V11)
boxplot(V32~V11)
boxplot(V33~V11)
boxplot(V34~V11)
boxplot(V35~V11)
boxplot(V36~V11)
boxplot(V37~V11)
boxplot(V38~V11)
boxplot(V39~V11)
boxplot(V40~V11)
boxplot(V41~V11)
boxplot(V42~V11)
boxplot(V43~V11)
boxplot(V44~V11)
boxplot(V45~V11)
dev.off()

# Impact of gender
pdf("boxplot1V8.pdf")
par(mfrow=c(2,4))
boxplot(V24~V8)
boxplot(V30~V8)
boxplot(V31~V8)
boxplot(V32~V8)
boxplot(V33~V8)
boxplot(V34~V8)
boxplot(V35~V8)
boxplot(V36~V8)
boxplot(V37~V8)
boxplot(V38~V8)
boxplot(V39~V8)
boxplot(V40~V8)
boxplot(V41~V8)
boxplot(V42~V8)
boxplot(V43~V8)
boxplot(V44~V8)
boxplot(V45~V8)
dev.off()

# Impact of 
pdf("boxplot1V50.pdf")
par(mfrow=c(2,4))
boxplot(V24~V50)
boxplot(V30~V50)
boxplot(V31~V50)
boxplot(V32~V50)
boxplot(V33~V50)
boxplot(V34~V50)
boxplot(V35~V50)
boxplot(V36~V50)
boxplot(V37~V50)
boxplot(V38~V50)
boxplot(V39~V50)
boxplot(V40~V50)
boxplot(V41~V50)
boxplot(V42~V50)
boxplot(V43~V50)
boxplot(V44~V50)
boxplot(V45~V50)
dev.off()
#Imparct of obesity
pdf("boxplot1V12.pdf")
par(mfrow=c(2,4))
boxplot(V24~V12)
boxplot(V30~V12)
boxplot(V31~V12)
boxplot(V32~V12)
boxplot(V33~V12)
boxplot(V34~V12)
boxplot(V35~V12)
boxplot(V36~V12)
boxplot(V37~V12)
boxplot(V38~V12)
boxplot(V39~V12)
boxplot(V40~V12)
boxplot(V41~V12)
boxplot(V42~V12)
boxplot(V43~V12)
boxplot(V44~V12)
boxplot(V45~V12)
dev.off()


#---------------------------------------------#
#     PART 3.4 : Outliers Detection           #
#---------------------------------------------#

quali_cols <- c("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23","V27","V28","V29","V50")

quanti_cols <- c("V24","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39"
                 ,"V40","V41","V42","V43","V44","V45")



# Mahalanobis distance
mean_vec <- colMeans(quanti_Data)
cova <- cov(quanti_Data)
maha <- mahalanobis(quanti_Data, mean_vec, cova)

temp <- as.data.frame(matrix(maha, ncol = 1))
temp$index = as.numeric(rownames(temp))

pdf("Mahalanobis distance.pdf")
plt <- ggplot(temp, aes(x = index, y = V1))
plt <- plt + geom_bar(stat = "identity")
plt <- plt + labs(x = "Index", y = "Mahalanobis distance")
plt <- plt + geom_hline(yintercept = qchisq(0.95, length(quanti_cols)), linetype = "dashed", color = "red")
plt
dev.off()




