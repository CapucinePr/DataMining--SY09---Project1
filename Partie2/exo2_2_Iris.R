setwd("C:/Users/Capucine/Documents/UTC/P18/SY09/Projet/donnees/donnees")

devtools::install_github("cran/mclust")
install.packages("mclust")
library(mclust)

#Iris
data(iris)
X <- iris[,1:4]
z <- iris[,5]

x.cr <- scale(X, center=T, scale=F)

X.acp <- princomp(X)
#Kmeans pour k =2 à 5
kmeansTab <- rep(0,times=5)

clus1 <- kmeans(x.cr,centers=1,nstart=100)
kmeansTab[1] <- clus1$tot.withinss 
#valeur du critère optimisé pour k=1 dans KmeansTab[1]

clus2 <- kmeans(x.cr,centers=2,nstart=100)
kmeansTab[2] <- clus2$tot.withinss 

clus3 <- kmeans(x.cr,centers=3,nstart=100)
kmeansTab[3] <- clus3$tot.withinss 

clus4 <- kmeans(x.cr,centers=4,nstart=100)
kmeansTab[4] <- clus4$tot.withinss 

clus5 <- kmeans(x.cr,centers=5,nstart=100)
kmeansTab[5] <- clus5$tot.withinss 

#681.37060 152.34795  78.85144  57.22847  46.44618
plot(1:5,kmeansTab,type="b",xlab="Nb. de groupes",ylab="inertie minimale")
#k=3 semble etre le meilleur

#Kmeans pour k =2 à 5
kmeansAdaTab <- rep(0,times=5)

clusAd1 <- KmeansAdaptative(x.cr,numberOfClasses=1,maxIterations=100,precision=0.25, numberOfEssays=10)
kmeansAdaTab[1] <- clusAd1$distanceTotal
#kmeansAdaTab[1] est le critère optimisé pour k=1

clusAd2 <- KmeansAdaptative(x.cr,numberOfClasses=2,maxIterations=100,precision=0.25, numberOfEssays=10)
kmeansAdaTab[2] <- clusAd2$distanceTotal

clusAd3 <- KmeansAdaptative(x.cr,numberOfClasses=3,maxIterations=100,precision=0.25, numberOfEssays=10)
kmeansAdaTab[3] <- clusAd3$distanceTotal  

clusAd4 <- KmeansAdaptative(x.cr,numberOfClasses=4,maxIterations=100,precision=0.25, numberOfEssays=10)
kmeansAdaTab[4] <- clusAd4$distanceTotal 

clusAd5 <- KmeansAdaptative(x.cr,numberOfClasses=5,maxIterations=100,precision=0.25, numberOfEssays=10)
kmeansAdaTab[5] <- clusAd5$distanceTotal 

#681.37060 100.70469  83.91213   0.00000  61.17442

plot(1:5,kmeansAdaTab,type="b",xlab="Nb. de groupes",ylab="distance totale")
#k=2 semble etre le meilleur

#acces aux valeurs en affichant KmeansAdaTAb et KmeansTab
#on a les classifications pour chaque k dans les clusAdK et clusK



#avec dendogramme
#CAH 
#matrice de distance
iris.cah <- dist(X, method="euclidean")
iris.cah.malh <- dist(X, method="malahanobis")

#critere d'agregation ward.D2, prend le carré de la distance
iris.cahWard <- hclust(iris.cah, method="ward.D2")
#show the dendogramme
plot(iris.cahWard)

#dendrogramme avec matérialisation des groupes 
rect.hclust(iris.cahWard,k=2)
rect.hclust(iris.cahWard,k=3)

#Partition pour K=2
X.K2 <- kmeans(x.cr, 2,10)
#2 classes de 97 et 53 éléments
#Cluster means:
#  Sepal.Length Sepal.Width Petal.Length Petal.Width
#1    0.4576976  -0.1707354     1.200763   0.4965430
#2   -0.8376730   0.3124780    -2.197623  -0.9087673
plot(X.acp$scores[,1], X.acp$scores[,2])
text(X.acp$scores[,1],X.acp$scores[,2], col=c("red","green")[X.K2$cluster], labels=z)
adjustedRandIndex(z, X.K2$cluster)
plot(X.)

X.K2.ada <- KmeansAdaptative(x.cr, 2, 10, 0.25, 10)
#2 classes de 81 et 69 éléments
#Sepal.Length  Sepal.Width Petal.Length
#[1,]  -0.06237654  0.007888889   -0.1820926
#[2,]   0.07322464 -0.009260870    0.2137609
#Petal.Width
#[1,] -0.09427778
#[2,]  0.11067391

plot(X.acp$scores[,1], X.acp$scores[,2])
text(X.acp$scores[,1],X.acp$scores[,2], col=c("red","green")[X.K2.ada$clusterIndex], labels=z)
adjustedRandIndex(z, X.K2.ada$clusterIndex)

#Partition pour K=3
X.K3 <- kmeans(x.cr, 3,10)
#K-means clustering with 3 clusters of sizes 38, 62, 50

#Cluster means:
#  Sepal.Length Sepal.Width Petal.Length Petal.Width
#1   1.00666667  0.01635088    1.9841053   0.8717193
#2   0.05827957 -0.30894624    0.6355484   0.2345376
#3  -0.83733333  0.37066667   -2.2960000  -0.9533333

plot(X.acp$scores[,1], X.acp$scores[,2])
text(X.acp$scores[,1],X.acp$scores[,2], col=c("red","green","blue")[X.K3$cluster], labels=z)


X.K3.ada <- KmeansAdaptative(x.cr, 3, 10, 0.25, 10)
#3 classes de 48, 51 et 51 éléments
plot(X.acp$scores[,1], X.acp$scores[,2])
text(X.acp$scores[,1],X.acp$scores[,2], col=c("red","green","blue")[X.K3.ada$clusterIndex], labels=z)


#On dirait que plus il y a de classes, plus kmeans ada a des groupes de même taille

#il faudrait plot les centers avec une couleur pour chaque méthode, histoire de montrer la répartition
