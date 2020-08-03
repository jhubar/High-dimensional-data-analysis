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
data <- data%>% select ("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                        "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                        "V27","V28","V29","V50","V24","V30","V31","V32","V33",
                        "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                        "V45")
quali_Data <- data %>% select("V1","V2","V3","V4","V6","V7","V8","V11","V12",
                              "V13","V14","V15","V16","V17","V19","V20","V21","V22","V23",
                              "V27","V28","V29","V50")
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}



quanti_Data <- data %>% select("V24","V30","V31","V32","V33",
                               "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                               "V45")
