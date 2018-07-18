setwd("C:/Users/Capucine/Documents/UTC/P18/SY09/Projet/donnees/donnees")

devtools::install_github("cran/mclust")
install.packages("mclust")
library(mclust)

#synth1
X <- read.csv("Synth1.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]


x.cr <- scale(X, center=T, scale=F)
#KMEANS
inertie.x.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- kmeans(x.cr,centers=k,nstart=100)
inertie.x.minmut[k] <- clus$tot.withinss }
plot(1:10,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="inertie minimale")

#2 ou 3 classes
class2 <- kmeans(x.cr, centers=2, nstart=100)
#K-means clustering with 2 clusters of sizes 101, 99

#Cluster means:
#  X.1       X.2
#1 -1.419303 -1.405738
#2  1.447976  1.434137

adjustedRandIndex(z,class2$cluster)

#KMEANS ADA
inertie.x.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- KmeansAdaptative(x.cr,numberOfClasses=k,maxIterations=100,precision=0.25, numberOfEssays=10)
inertie.x.minmut[k] <- clus$distanceTotal }
plot(1:10,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="distance totale")

#tableau des critères
x.ine.ada <- rep(0, times=5)

x.ine.adaT<-KmeansAdaptative(x.cr,numberOfClasses=1,maxIterations=100,precision=0.25, numberOfEssays=10)
x.ine.ada[1] <- x.ine.adaT[[3]]
x.ine.adaT2<-KmeansAdaptative(x.cr,numberOfClasses=2,maxIterations=100,precision=0.25, numberOfEssays=10)
x.ine.ada[2] <- x.ine.adaT2$distanceTotal
x.ine.adaT<-KmeansAdaptative(x.cr,numberOfClasses=3,maxIterations=100,precision=0.25, numberOfEssays=10)
x.ine.ada[3] <- x.ine.adaT$distanceTotal
x.ine.adaT<-KmeansAdaptative(x.cr,numberOfClasses=4,maxIterations=100,precision=0.25, numberOfEssays=10)
x.ine.ada[4] <- x.ine.adaT$distanceTotal
x.ine.adaT<-KmeansAdaptative(x.cr,numberOfClasses=5,maxIterations=100,precision=0.25, numberOfEssays=10)
x.ine.ada[5] <- x.ine.adaT$distanceTotal

plot(1:5,x.ine.ada,type="b",xlab="Nb. de groupes",ylab="distance totale")
#choisir le K

#Comparaison KmeansAda et classification réelle
adjustedRandIndex(z, x.ine.adaT2$clusterIndex)

#synth2
X <- read.csv("Synth2.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]


x.cr <- scale(X, center=T, scale=F)

inertie.x.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- kmeans(x.cr,centers=k,nstart=100)
inertie.x.minmut[k] <- clus$tot.withinss }
plot(1:10,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="inertie minimale")

#KMEANS ADA
inertie.x.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- KmeansAdaptative(x.cr,numberOfClasses=k,maxIterations=100,precision=0.25, numberOfEssays=10)
inertie.x.minmut[k] <- clus$distanceTotal }
plot(1:10,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="distance totale")

class2 <- kmeans(x.cr,centers=2,nstart=100)

adjustedRandIndex(z,class2$cluster)

classKmeansAd2 <-KmeansAdaptative(x.cr,numberOfClasses=2,maxIterations=100,precision=0.25, numberOfEssays=10)

adjustedRandIndex(z,classKmeansAd2$clusterIndex)

#synth3
X <- read.csv("Synth3.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]


x.cr <- scale(X, center=T, scale=F)

inertie.x.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- kmeans(x.cr,centers=k,nstart=100)
inertie.x.minmut[k] <- clus$tot.withinss }
plot(1:10,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="inertie minimale")

#KMEANS ADA
inertie.x.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- KmeansAdaptative(x.cr,numberOfClasses=k,maxIterations=100,precision=0.25, numberOfEssays=10)
inertie.x.minmut[k] <- clus$distanceTotal }
plot(1:10,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="distance totale")

class2 <- kmeans(x.cr,centers=2,nstart=100)

adjustedRandIndex(z,class2$cluster)

classKmeansAd2 <-KmeansAdaptative(x.cr,numberOfClasses=2,maxIterations=100,precision=0.25, numberOfEssays=10)

adjustedRandIndex(z,classKmeansAd2$clusterIndex)
