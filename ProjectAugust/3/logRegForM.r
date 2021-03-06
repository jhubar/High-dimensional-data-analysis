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
library(FactoMineR)
library(factoextra)
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


data <- data[V1=='1',]

quantData <- data %>% dplyr::select("V24","V30","V31","V32","V33",
                                    "V34","V35","V36","V37","V38","V39","V40","V41","V42","V43","V44",
                                    "V45")

quanti_var <- c('V24','V30','V31','V32','V33',
                'V34','V35','V36','V37','V38','V39','V40','V41','V42','V43','V44',
                'V45')

#---------------------------------------------#
#         Logistic Regression Model           #
#---------------------------------------------#

#---------------------------------------------#
#       Search a good logistic model          #
#---------------------------------------------#

explanatory_var <- quanti_var

#---------------------------------------------#
#               Complete model                #
#---------------------------------------------#
glm1 <- glm(V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45, family = binomial(link = "logit"))

summary(glm1)
plot(glm1$linear.predictors,fit1$fitted ,col = colors.th)

#---------------------------------------------#
#       Draw correlation plot of the          #
#            explanatory variables            #
#---------------------------------------------#

corrplot_of_explanatory_variables <- cor(data[, explanatory_var])
pdf("correlation_plot_of_explanatory_variables.pdf")
corrplot(corrplot_of_explanatory_variables, method = "circle", type = "lower", tl.col = "black", tl.pos = "ld", tl.srt = 45)
dev.off()
#---------------------------------------------#
#       Compute VIF scores to check           #
#          for multicolinearity               #
#---------------------------------------------#

VIF_scores <- vif(glm1)
print(VIF_scores)
print(sort(VIF_scores))
pdf("vif_scores.pdf")
grPal <- colorRampPalette(c('green','orange','red'))
Col <- grPal(3)[as.numeric(cut(VIF_scores,breaks = 3))]
plot(VIF_scores,pch = 20,col=Col)
abline(h=mean(c(3.791036,2.966739)), col= "red")
abline(h=mean(c(2.165801,1.702041)), col= "orange")
dev.off()

#---------------------------------------------#
#       Compute glmV38,39,40,41               #
#---------------------------------------------#
glmV38 <- glm(V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V38+V42+V43+V44+V45, family = binomial(link = "logit"))
summary(glmV38)
glmV39 <- glm(V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V39+V42+V43+V44+V45, family = binomial(link = "logit"))
summary(glmV39)
glmV40 <- glm(V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V40+V42+V43+V44+V45, family = binomial(link = "logit"))
summary(glmV40)
glmV41 <- glm(V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V41+V42+V43+V44+V45, family = binomial(link = "logit"))
summary(glmV41)
#---------------------------------------------#
#       PCA(V38,39,40,41) -> model            #
#---------------------------------------------#
pca <- princomp(data[c('V38', 'V39', 'V40','V41')])
fviz_pca_var(res.pca, col.var = "black")
res.pca <- prcomp(data[c('V38', 'V39', 'V40','V41')], scale = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
explanatory_var_updated <- c('V24','V30','V31','V32','V33','V34','V35','V36','V37','V42','V43','V44','V45', 'PC1')
data_Updated <- data[c('V24','V30','V31','V32','V33','V34','V35','V36','V37','V42','V43','V44','V45','V50')]
data_Updated['PC1'] <- pca$scores[, 1]
attach(data_Updated)

glm2 <- glm(V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V42+V43+V44+V45+PC1, family = binomial(link = "logit"))
summary(glm2)

# Conduct variable selection using backward selection.
stepAIC(glm2)
plot(glmSuper)
dev.off()
# Conduct variable selection using forward selection.
model_null <- glm(V50 ~ 1, family = binomial(link = "logit"))
stepAIC(model_null, scope = V50 ~ V24+V30+V31+V32+V33+V34+V35+V36+V37+V42+V43+V44+V45+PC1, direction = "forward")

# Remove V30, V31, V33, V34, V35, V42, V43, V44 from model
model_updated <- glm(V50 ~ V24 + V32 + V36 + V37 + V45 + PC1,family = binomial(link = "logit"))
explanatory_var_updated <- c('V24', 'V32', 'V36', 'V37', 'V45','PC1')

res.pca_model_final <- prcomp(data_Updated[c('V24', 'V32', 'V36', 'V37', 'V45','PC1')], scale = TRUE)
fviz_eig(res.pca_model_final)
pdf("circle_pca.pdf")
fviz_pca_var(res.pca_model_final,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
dev.off()


#----------------------------------------#
#                 Predict                #
#----------------------------------------#
predictions <- (predict.glm(object = model_updated, newdata = data_Updated[, explanatory_var_updated], type = "response") > 0.5)
errors <- (data_Updated['V50'] != predictions) 
mean(errors)
#----------------------------------------#
#              fitted values             #
#----------------------------------------#
fitted_values <- model_updated$fitted.values
summary(fitted_values)
plt <- ggplot(data.frame(fitted_values), aes(y = fitted_values)) + geom_boxplot()
plt <- plt + labs(x = "", y = "Fitted values")
ggsave("boxplots_fitted_values.pdf", plt)

#----------------------------------------#
#     Look at Pearson residuals          #
#----------------------------------------#
residuals <- resid(model_updated, type = "pearson")
summary(residuals)
plt <- ggplot(data.frame(residuals), aes(y = residuals)) + geom_boxplot()
plt <- plt + labs(x = "", y = "Pearson residuals")
ggsave("boxplots_residuals.pdf", plt)

#----------------------------------------#
#     CV with model_Updated              #
#----------------------------------------#


LOO_Post<- rep(0, dim(data_Updated)[1])
for (i in 1:dim(data_Updated)[1]) {
  idx <- rep(TRUE, dim(data_Updated)[1])
  idx[i] <- FALSE
  linear_classification <- glm(V50[idx] ~ V24[idx] + V32[idx] + V36[idx] + V37[idx] + V45[idx] + PC1[idx], family = binomial(link = "logit"))
  prediction <- linear_classification$coefficients %*% c(1, as.matrix(data_Updated[i, explanatory_var_updated]))
  LOO_Post[i] <- exp(prediction)/(1 + exp(prediction))
}

# Construct confusion matrix
threshold = 0.5
Actual<- V50
Predicted <- as.integer(LOO_Post> threshold)
conf_mat <- table(V50,Predicted)


# Compute sensitivity and specificity
sens_or_spec <- function(confusion_matrix, spec = FALSE) {
  if (spec) {
    specificity <- (confusion_matrix[1,1])/(confusion_matrix[1,1] + confusion_matrix[1,2])
    return(specificity)
  }
  else {
    sensitivity <- (confusion_matrix[2,2])/(confusion_matrix[2,2] + confusion_matrix[2,1])
    return(sensitivity)
  }
}
sensitivity <- sens_or_spec(conf_mat)
print(sensitivity)
specificity <- sens_or_spec(conf_mat, spec = TRUE)
print(specificity)

# Plot ROC curve
ROC_maker <- function(scores, memberships) {
  cutoff <- sort(scores)[1:(length(scores)-1)]
  sensitivity <- rep(0, length(cutoff))
  specificity <- rep(0, length(cutoff))
  idx <- 1
  for (cut in cutoff) {
    data <- as.data.frame(scores)
    data["Membership"] <- as.integer(data > cut)
    
    confusion_matrix <- table(memberships, data[, "Membership"])
    sensitivity[idx] <- sens_or_spec(confusion_matrix)
    specificity[idx] <- sens_or_spec(confusion_matrix, spec = TRUE)
    idx <- idx + 1
  }
  sensitivity <- c(sensitivity, 0)
  specificity <- c(specificity, 1)
  plot(1 - specificity, sensitivity, type="l")
  
  return(list(spec = (1 - specificity), sens = sensitivity))
}
#pdf("ROC.pdf")
ROC <- ROC_maker(LOO_Post, V50)
abline(a = 0, b = 1, col = "red")
abline( v = 1-specificity, col = "blue")
abline( h= sensitivity , col = "blue")
dev.off()
# Compute AUC
AUC <- function(ROCx, ROCy)
{
  n <- length(ROCx)
  base <-ROCx[1:(n-1)] -ROCx[2:n] 
  height <- ROCy[2:n]
  return(sum(base*height))
}
auc <- AUC(ROCx = ROC$spec, ROCy = ROC$sens)
print(auc)
#dev.off()

# Compute AUC
AUC <- function(ROCx, ROCy)
{
  n <- length(ROCx)
  base <-   ROCx[1:(n-1)])-ROCx[2:n]
  height <- ROCy[2:n]
  return(sum(base*height))
}
auc <- AUC(ROCx = ROC$spec, ROCy = ROC$sens)
print(auc)