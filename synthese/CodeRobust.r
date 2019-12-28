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
setwd("/Users/Julien/Documents/#Master1/HDAD/cours/synthese")
data<-read.csv("HappyWorld.csv",header= TRUE, sep= ";" )
data.With.Na <- data
data<-na.omit(data)
attach(data)
summary(data)
#---------------------- Treshold -----------------------------------------------

data$LifeLadder.th <- data.frame(data$LifeLadder)
data <- data %>%
  mutate(LifeLadder.th  = case_when (LifeLadder.th >= 5.0 ~ 1, LifeLadder.th < 5.0 ~ 0))



#---------------------- Graphical model -----------------------------------------

# A) ----------------------------------------------------------------------------
# --------------------------- classic var-cov matrix ----------------------------
# Pay attention when each data are not express in same measure, you must use
# correlation matrix.

m.cov <- cov(data[,3:12])
corrplot::corrplot(m.cov,title = "classic covariance matrix")
corrplot::corrplot.mixed(m.cov,title = "classic covariance matrix")

#---------------------- classic correlation matrix ------------------------------

m.cor <- cor(data[,3:12])
corrplot::corrplot(m.cor,title = "classic correlation matrix")
corrplot::corrplot.mixed(m.cor,title = "classic correlation matrix")

# ---------------------- Detection of outliers ---------------------------------

# Classic mahanalobis distance 

m <- apply(data[,3:12],2,mean)
d <- mahalanobis(data[,3:12],m,m.cov)
plot(d, main = " classic mahanalobis distance ")
abline(h = qchisq(0.95,11),col = 'red')

# Robust covariance matrix
cov.rob <- cov.rob(data[,3:12],method = "mcd", cor =TRUE)
m.cov.rob <- cov.rob$cov
corrplot::corrplot(m.cor.rob, title = "Robust covariance matrix")

# Robust correlation matrix 
m.cor.rob <- cov.rob$cor
corrplot::corrplot(m.cor.rob, title = "Robust correlation matrix")

# Robust mahanalobis distance 
d2 <- mahalanobis(data[,3:12],cov.rob$center,m.cov.rob)
plot(d2, main = "Robust mahanalobis distance")
abline(h= qchisq(0.95,11),col='red')

#DDplot 
plot(d, d2, xlab = "mahalanobis distance", ylab = "robust distance")
abline(h= qchisq(0.95, 11) , col='red')
abline(v = qchisq(0.95, 11), col = 'green')

# Outliers
data.outlier <- data[which(d > qchisq(0.95, 11)),]
data.outlier2 <- data[which(d2 > qchisq(0.95, 11)) ,]
data.rob <- data[which(d < qchisq(0.95, 11)),]
data.rob2 <- data[which(d2 < qchisq(0.95, 11)),]


#------------------------------- qgraph ------------------------------------------------------


m.inv.cov <- matlib::inv(m.cov)   


par(mfrow=c(4,2))
qgraph(m.inv.cov,diag = FALSE,fade = FALSE)
mglasso.0 <-glasso(m.cov, rho = 0)
qgraph(mglasso.0,fade =FALSE)
mglasso.0.005 <-glasso(m.cov, rho = 0.005)
qgraph(mglasso.0.005,fade =FALSE)
mglasso.0.05 <-glasso(m.cov, rho = 0.05)
qgraph(mglasso.0.05,fade =FALSE)
mglasso.0.5 <-glasso(m.cov, rho = 0.5)
qgraph(mglasso.0.5,fade =FALSE)
mglasso.1 <-glasso(m.cov, rho = 1)
qgraph(mglasso.1,fade = FALSE)
par(mfrow=c(1,1))


qgraph(m.cor)



#-------------------------------PCA-----------------------------------------------------------

res.pca <- PCA(data[,3:12],scale.unit = TRUE, ncp= 10, graph = TRUE, covmat = ) 
#scale.unit: Si TRUE, les donnée sont standardisée/normalisée avant l'analyse.
print(res.pca)
#valeur propres et variances. 
eig.val <- get_eigenvalue(res.pca)
eig.val
#Le graphique des valeurs propres peut etre généré à l'aide de la fonction fviz_eig() ou fviz_screeplot() 
fviz_eig(res.pca, addlabels = TRUE,ylim = c(0,50))
#resulat
var <- get_pca_var(res.pca)
#coordonées 
head(var$coord)
# cos2: qualité de répresentation
head(var$cos2)
# contrinbution aux composantes principales
head(var$contrib)

BF <- c ("France","Belgium")
fviz_pca_ind(res.pca, col.ind = (data$country == BF))


#-------------------------------TSE-----------------------------------------------------

library("Rtsne")
#color by row names

labels.row <- names(data[,3:12])
data$labels.row <- as.factor(labels.row)

colors.row <- rainbow(length(unique(data$labels.row)))
names(colors.row) = unique(data$labels.row)


# tSNE visualization with perplexity = 5

tsne <- Rtsne(data[,3:12], dims = 2, perplexity = 20, verbose = TRUE, max_iter = 500)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne1 <- ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour= data$labels.row)) +
  labs(
    title = "2D tSNE visualization with perplexity = 20",
    colour = labels.row,
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )

# tSNE visualization with perplexity = 20
tsne <- Rtsne(data[,3:12], dims = 2, perplexity = 20, verbose = TRUE, max_iter = 500)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne2 <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour=  data$labels.row)) +
  labs(
    title = "2D tSNE visualization with perplexity = 20",
    colour = labels.row,
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )


# tSNE visualization with perplexity = 30
tsne <- Rtsne(data[,3:12], dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne3 <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour=  data$labels.row)) +
  labs(
    title = "2D tSNE visualization with perplexity = 30",
    colour = labels.row,
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )


# tSNE visualization with perplexity = 50
tsne <- Rtsne(data[,3:12], dims = 2, perplexity = 50, verbose = TRUE, max_iter = 500)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne4 <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour= data$labels.row)) +
  labs(
    title = "2D tSNE visualization with perplexity = 50",
    colour = labels.row,
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )


ggarrange(tsne1,tsne2, tsne3,tsne4,ncol = 2, nrow = 2)
ggsave("tnse.png")



# tSNE visualization with perplexity = 5
tsne <- Rtsne(data[,3:12], perplexity = 5)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne1.th <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour=data$LifeLadder.th)) +
  labs(
    title = "2D tSNE visualization with perplexity = 55",
    colour ="LifeLadder.th",
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )
# tSNE visualization with perplexity = 20
tsne <- Rtsne(data[,3:12], perplexity = 20)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne2.th <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour=data$LifeLadder.th)) +
  labs(
    title = "2D tSNE visualization with perplexity = 50",
    colour ="LifeLadder.th",
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )
# tSNE visualization with perplexity = 30
tsne <- Rtsne(data[,3:12], perplexity = 30)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne3.th <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour=data$LifeLadder.th)) +
  labs(
    title = "2D tSNE visualization with perplexity = 50",
    colour ="LifeLadder.th",
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )
# tSNE visualization with perplexity = 50
tsne <- Rtsne(data[,3:12], perplexity = 50)
df_tsne5 <- data.frame(comp1 = tsne$Y[,1], comp2 = tsne$Y[,2] )
tsne4.th <-ggplot() +
  geom_point(data = df_tsne5, aes(x = df_tsne5$comp1, y = df_tsne5$comp2, colour=data$LifeLadder.th)) +
  labs(
    title = "2D tSNE visualization with perplexity = 50",
    colour ="LifeLadder.th",
    x = "tSNE comp 1",
    y = "tSNE comp 2"
  )
ggarrange(tsne1.th,tsne2.th, tsne3.th,tsne4.th,ncol = 2, nrow = 2)
ggsave("tsne.th.png")


#------------------------------ RegLog ----------------------------------------------------------

df <- data %>%
  dplyr::select(-country, - LifeLadder, -labels.row)


fit1 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit1)
plot(fit1$linear.predictors,fit1$fitted ,col = colors.th)

df <- df %>%
  dplyr::select(- Corruption)

fit2 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit2)
plot(fit2$linear.predictors,fit2$fitted ,col = colors.th)


df <- df %>%
  dplyr::select(- Generosity)

fit2 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit2)
plot(fit2$linear.predictors,fit2$fitted ,col = colors.th)


df <- df %>%
  dplyr::select(- Positiveaffect)

fit2 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit2)
plot(fit2$linear.predictors,fit2$fitted ,col = colors.th)


df <- df %>%
  dplyr::select(- LifeExp)

fit2 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit2)
plot(fit2$linear.predictors,fit2$fitted ,col = colors.th)


df <- df %>%
  dplyr::select(- Negativeaffect)

fit2 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit2)
plot(fit2$linear.predictors,fit2$fitted ,col = colors.th)


df <- df %>%
  dplyr::select(- Confidencegovernment)

fit2 <- glm(LifeLadder.th ~ ., data = df, family = binomial(logit))

summary(fit2)
plot(fit2$linear.predictors,fit2$fitted ,col = colors.th)



