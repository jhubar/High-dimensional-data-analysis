  setwd("/home/delvoye/Dropbox/01_Ingé/MasterI/01_QI/HDDA/Projects/projet_aout/P1")
  #---------------------------------------------#
  #                 Library                     #
  #---------------------------------------------#
  install.packages(seaborn)
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
  library(seaborn)
    data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
    quanti_Data <- data %>% select("V24","V30","V31","V32","V33",
                                   "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                                   "V45")
    for(i in 1:ncol(quanti_Data)){
      quanti_Data[is.na(quanti_Data[,i]), i] <- mean(quanti_Data[,i], na.rm = TRUE)
    }
    

    # z-score calculation
    z <-scale(quanti_Data)
    mz <- melt(z)
    plt <- ggplot(mz, aes(x = Var2, y = value))
    plt <- plt + geom_point(alpha = 1/3)
    plt <- plt + labs(x = "Variable", y = "Z-score")
    plt <- plt + geom_hline(yintercept = 1.96, linetype = "dashed", color = "red")
    plt <- plt + geom_hline(yintercept = -1.96, linetype = "dashed", color = "red")
    ggsave("z_scores.pdf", plt)
    
    mean_vec <- colMeans(quanti_Data)
    cova <- cov(quanti_Data)
    maha <- mahalanobis(quanti_Data, mean_vec, cova)
    
    temp <- as.data.frame(matrix(maha, ncol = 1))
    temp$index = as.numeric(rownames(temp))
    
    plt <- ggplot(temp, aes(x = index, y = V1))
    plt <- plt + geom_bar(stat = "identity")
    plt <- plt + labs(x = "Index", y = "Mahalanobis distance")
    plt <- plt + geom_hline(yintercept = qchisq(0.95, length(quanti_Data)), linetype = "dashed", color = "red")
    ggsave("maha.pdf", plt)
