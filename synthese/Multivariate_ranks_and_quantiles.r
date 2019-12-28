setwd("/Users/Julien/Documents/#Master1/HDAD/cours/synthese")
#Function to compute the mahalanobis depth function
maha_depth <- function(x, m = colMeans(x), S=cov(x), rob = FALSE)
{
  if(isTRUE(rob))
  {
    S <- cov.rob(x, method = "mcd")
  }
  1/(1+mahalanobis(x,m,S))
}

#Uniform distribution EX2
library(depth)
library(MASS)
x <- cbind(runif(1000, -1, 1), runif(1000, -2, 2))
plot(x)

depth_value_Tukey <- apply(x, 1, function(u){depth(u, x)})
depth_values_simplicial <- apply(x, 1, function(u){depth(u, x, method="Liu")})
depth_values_maha <- maha_depth(x)

plot(x, col=topo.colors(length(unique(depth_value_Tukey)))[as.factor(depth_value_Tukey)],pch=16, main="Tukey Depth")
plot(x, col=topo.colors(length(unique(depth_values_simplicial)))[as.factor(depth_values_simplicial)],pch=16, main="Liu Depth")
plot(x, col=topo.colors(length(unique(depth_values_maha)))[as.factor(depth_values_maha)],pch=16, main="Mahalanobis Depth")

a <- seq(-1, 1, l=100)
b <-seq(-2, 2, l=100)
z <- matrix(NA, 100, 100)
y <- matrix(NA, 100, 100)
w <- matrix(NA, 100, 100)
for(i in 1:100)
  for(j in 1:100){
    z[i,j] <- depth(c(a[i],b[j]),x)
    y[i,j] <- depth(c(a[i],b[j]),x, method = "Liu")
    w[i,j] <- maha_depth(c(a[i],b[j]), colMeans(x), cov(x))
  }
contour(a,b,z, main = "Tukey Contour")
contour(a,b,y, main = "Liu Contour")
contour(a,b,w, main = "Mahalanobis Contour")

#Rank comparision EX3
norm_cor = c(100)
unif_cor = c(100)
for (i in 1:100){
  
  sigma <- matrix(c(1,1/2,-1,1/2,2,1/4,-1,1/4,3),nrow = 3,ncol=3, byrow = TRUE)
  normal_data <- mvtnorm::rmvnorm(n=100,mean = c(0,0,0), sigma = sigma )
  uniform_data <- cbind(runif(100, -1, 1),runif(100, -2, 2),runif(100, -3, 3))

  DTukey_normal <- apply(normal_data, 1, function(u){depth(u, normal_data)})
  DMaha_normal <- maha_depth(normal_data)
  DTukey_uniform <- apply(uniform_data, 1, function(u){depth(u, uniform_data)})
  DMaha_uniform <- maha_depth(uniform_data)

  norm_cor[i]<-cor(DTukey_normal,DMaha_normal,method = "spearman")
  unif_cor[i]<-cor(DTukey_uniform,DMaha_uniform,method = "spearman")
}

boxplot(norm_cor,unif_cor)

#EX4
data <- read.table("car.txt", header=TRUE,sep = ",")
col = c(10,11,12,13,14,17,19,20,21,22,23,24,25,26)
num_data <- na.omit(data[col])
attach(num_data)
#A
depth_value_Tukey <- apply(num_data, 1, function(u){depth(u, num_data)})
depth_value_Maha <- maha_depth(num_data)
which(depth_value_Tukey == max(depth_value_Tukey))
which(depth_value_Maha == max(depth_value_Maha))

#B
library(aplpack)
bag<-bagplot(Length,Height)

#C
regularized_cov <- function(S, lambda){

  eig = eigen(S)
  (values <- eig$values)
  (vectors <- eig$vectors)

  theta_pml <- 0
  theta_i <- (-values + sqrt(values^2+ 8*lambda))/(4*lambda)

  theta_pml <- (theta_i * vectors) %*% t(vectors)

  res <- list(icov = theta_pml, loglik = log(det(theta_pml)) -sum(diag(S %*% theta_pml)) )
  return(res)
}

detach(num_data)
subaru <- data[which(data$Make == "subaru"),]
subaru <- na.omit(subaru[col])
attach(subaru)
depth_subaru <- maha_depth(subaru,colMeans(subaru),regularized_cov(cov(subaru),0.5)$icov)
rank(sort(depth_subaru, decreasing = TRUE))

#D
detach(subaru)
toyota <- data[which(data$Make == "toyota"),]
toyota <- na.omit(toyota[col])
attach(toyota)
depth_toyota <- maha_depth(toyota)
rank(sort(depth_toyota, decreasing = TRUE))
depth_toyota <- maha_depth(toyota,colMeans(toyota),regularized_cov(cov(toyota),0.5)$icov)
rank(sort(depth_toyota, decreasing = TRUE))
