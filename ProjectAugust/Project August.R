

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
#---------------------------------------------#
#           Data pre-processing               #
#---------------------------------------------#
data <- hcc.data
attach(data)

#data %>%
#  rename(
#    V1 = Gender,
#    V2 = Symptoms,
#    V3 = Alcohol,
#    V4 = Hepatitis_B_Surface_Antigen,
#    V5 = Hepatitis_B_e_Antigen,
#    V6 = Hepatitis_B_Core_Antibody,
#    V7 = Hepatitis_C_Virus_Antibody,
#    V8 = Cirrhosis,
#    V9 = Endemic, 
#  )


#---------------------------------------------#
#             Part 2: Missingness             #
#---------------------------------------------#

# Missingness visualisation
pdf("/Users/julienhubar/Documents/#Master1/HDDA/ProjectAugust/MissingnessVisualisation.pdf")
vis_miss(data, sort_miss = TRUE)
dev.off()

## Missingness 
columns <- colnames(data)
columns_miss <- columns[colSums(is.na(data)) > 0]
#pdf("/Users/julienhubar/Documents/#Master1/HDDA/ProjectAugust/missingness_patterns.pdf")
gg_miss_upset(data, nset = length(columns_miss))
#dev.off()

## Missingness rate for each columns
#pdf("/Users/julienhubar/Documents/#Master1/HDDA/ProjectAugust/missingness_rates.pdf")
nb_na <- colSums(is.na(data[, columns_miss]))
barplot(nb_na / nrow(data), legend.text = nb_na, col = rainbow_hcl(length(columns_miss)))
#dev.off()

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
#      Part 3: Explanatory analysis           #
#---------------------------------------------#

quanti_Data <- data %>%
  select()
quali_Data <- data %>% select()

#---------------------------------------------#
#  Part 3.1: Univariate exploratory analysis  #
#---------------------------------------------#
#summary(quanti_Data)

#melted <- melt(quanti_Data)
#plt <- ggplot(melted, aes(x = value)) + geom_histogram()
#plt <- plt + facet_wrap( ~ variable, scales = "free") + labs(x = "", y = "")
#ggsave("/Users/julienhubar/Documents/#Master1/HDDA/ProjectAugust/histograms.pdf", plt)

#---------------------------------------------#
# Part 3.2: Multivariate exploratory analysis #
#---------------------------------------------#

#corr <- data
#pdf("/Users/julienhubar/Documents/#Master1/HDDA/ProjectAugust/correlation.pdf")
#corrplot(corr, method = "color", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
#dev.off()
#---------------------------------------------#
# PART 3.3 : Qualitative variables impact on  #
#            quantitative one                 #
#---------------------------------------------#

#---------------------------------------------#
#     PART 3.4 : Outliers Detection           #
#---------------------------------------------#


