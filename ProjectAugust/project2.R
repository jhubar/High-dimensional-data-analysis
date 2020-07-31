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
library(MASS)
library(hexbin)
library(readtext)


data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
attach(data)

#---------------------------------------------#
#     Strategy to treat missing data          #
#---------------------------------------------#


for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#---------------------------------------------#
#       Robust outlier detection              #
#---------------------------------------------#
quanti_Data <- data %>% select("V24","V25","V26","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39"
                               ,"V40","V41","V42","V43","V44","V45","V46","V47","V48","V49")

## Use MCD estimator (Coverage parameter of 0.75)
h <- floor((dim(quanti_Data)[1] + dim(quanti_Data)[2] + 1)/2)
set.seed(0)
robust <- cov.rob(quanti_Data, cor = TRUE, quantile.used = h, method = "mcd")

