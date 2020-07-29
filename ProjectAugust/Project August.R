

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

#set.seed(0)
#data <- data[sample.int(nrow(data), 500), ]
#data <- data %>%
#  mutate(sex = case_when(sex == 'male' ~ 1,sex == 'female' ~ 0 )) %>%
#  mutate(age = case_when(age == '10s' ~ 10,age == '20s' ~ 20,age == '30s' ~ 30,age == '40s' ~ 40,age == '50s' ~ 50,
#                         age == '60s' ~ 60,age == '70s' ~ 70,age == '80s' ~ 80,age == '90s' ~ 90))
#mean(data$sex,na.rm = TRUE)
#summary(data)
#attach(data)
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

#quanti_Data <- data %>%
#  select("patient_id","sex","age","infected_by","contact_number","symptom_onset_date","avg_temp","min_temp","max_temp",
#        "precipitation","max_wind_speed","most_wind_direction","avg_relative_humidity","code")
#quali_Data <- data %>% select("country","province","city","infection_case","state","symptom_onset_date","confirmed_date","deceased_date")

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


