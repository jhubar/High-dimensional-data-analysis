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
setwd("/Users/Julien/Documents/#Master1/HDAD/cours")
data <- read.table("/Users/Julien/Documents/#Master1/HDAD/cours/car.txt", sep=",", header = TRUE)
attach(data)

#TP1
# Quantitative variable with no difference in the means in the groups defined by M: -----------------------
    # On cree un vecteur numerique avec LOSS
    # Dans l'objectif de faire un t.test on va regarder les moyenes de Height quand M est NUlL
    # Et quand M ne les pas.
    
    M <- as.numeric(is.na(Loss))
    ggplot(data, aes(x= Loss,y=Height))+geom_miss_point()
    # on peut voir que l'hypothese de normalité dans le group 0 ne tien pas.
    boxplot(Height ~ M)
    t.test(Height[M==0],Height[M==1], var.equal=FALSE)
    # La valeur de P = 0.607 > a: La différence entre les moyennes n'est pas statistiquement significative
    # impossible de rejeter H0
    
# Quantitative variable with difference in the means in the groups defined by M: -----------------------
    
    ggplot(data, aes(y = Length,x=Loss))+geom_miss_point()
    # on peut voir que l'hypothese de normalité dans le group 0 ne tien pas.
    boxplot(Length ~ M)
    t.test(Length[M==0],Length[M==1], var.equal=FALSE)
    # La valeur de P = 0.0001909 <= a: La différence entre les moyennes est statistiquement significative
    # (on rejete HO)

# Qualitative variable with no difference in behaviors in the groups defined by M : --------------------
    
    table(M,Fuel) #Contingency table
    addmargins(prop.table(table(M, Fuel), margin=2), margin=1) #Marginal table
    # Proportion of missing values are quite the same for each type of fue. 
    fisher.test(M, Fuel) # test d'indépendance
    # La valeur de P = 0.5599 : On ne peut donc rejeter l'hypothese d'indépendance. 
    
# Qualitative variable with difference in behaviors in the groups defined by M: ------------------------
    
    table(M, DriveW) #Contingency table
    addmargins(prop.table(table(M, DriveW),margin = 2),margin=1) #Marginal table
    # Proportions of missing values are different in the groups defined by the DriveW variable
    fisher.test(M,DriveW) # test d'indépendance 
    # la valeur de P = 0.0006362: on peut rejeter l'hypothese d'indépendance.
    



    

