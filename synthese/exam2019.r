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


setwd("/Users/Julien/Documents/#Master1/HDAD/cours/synthese")
data<-read.csv("HappyWorld.csv",header= TRUE, sep= ";" )
attach(data)
summary(data)


par(mfrow=c(3,4))
for( i in 2:12){
  plot(data[,i],col = country)
}
par(mfrow=c(1,1))


summary(data)

for (i in 2:12){
  var.tmp <- data[,i]
  mean.tmp <- mean(data[,i],na.rm = TRUE)
  var.tmp[is.na(var.tmp)] <- mean.tmp
  data[,i] <- var.tmp
}

barplot( LifeLadder~country )

par(mfrow=c(3,4))
for (i in 3:12){
  barplot( data[,i]~data$country)
}
par(mfrow=c(1,1))

df <- data %>%
  dplyr::select(-country,-LifeLadder)

summary(df)
m.df <- as.matrix(df)

pca <- prcomp(df,scale = TRUE)
plot(pca$x[,1],pca$x[,2])
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
par(mfrow=c(1,2))
corrplot::corrplot(cor(df))
plot(pca,type="l",main= " ")
par(mfrow=c(1,1))

library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])

top_10_genes ## show the names of the top 10 genes

pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)


#Le résumé des performances de l'ACP 
pca.var.per[1]+pca.var.per[2]
pca.var.per[1]+pca.var.per[2]+pca.var.per[3] 
#Graphique de corrélation.
fviz_pca_var(pca, col.var = "black",ncp = 5)

