setwd("C:/Users/Capucine/Documents/UTC/P18/SY09/Projet/donnees/donnees")

devtools::install_github("cran/outliers")
install.packages("outliers")
library(outliers)

#SPAM
Spam <- read.csv("spam.csv", header=T, row.names=1)
X <- Spam[,-58]
z <- Spam[,58]

x.cr <- scale(X, center=T, scale=F)

length(which(z==1))
#1813 elements dans la classe 1

length(which(z==2))
#2788 éléments dans la classe 2

clus3 <- kmeans(x.cr,centers=3,nstart=100)
kmeansTab[3] <- clus3$tot.withinss 

clus4 <- kmeans(x.cr,centers=4,nstart=100)
kmeansTab[4] <- clus4$tot.withinss 

clus5 <- kmeans(x.cr,centers=5,nstart=100)
kmeansTab[5] <- clus5$tot.withinss 

x.acp <- princomp(X)
plot(x.acp$scores, col=z)

X.out <- scores(X, prob=0.95)
X.out <- which(X.out==FALSE)

#fonction pour trouver les outliers et les supprimer ensuite
#on ajoute la colonne z des bonnes lignes
deleteOutliers <- function(x,z){
  y <- x  
  for(k in 1:ncol(y))
  {
    id1 <- boxplot.stats(y[,k])
    nr <- nrow(y)
    for(j in 1:nr)
    {
      if(j<=nr){
      if((is.na(y[j,k])) || (y[j,k]<id1$stats[1]) || (y[j,k]>id1$stats[5]))
      {
        y <- y[-j,]
        z <- z[-j]
        nr <- nr-1
      }}
    }

  }
  c <- cbind(y,z)
  return(c)
}

#y sera sans les outliers
y <- deleteOutliers(x.cr)
#on a plus que 189 éléments


#si on fait sur l'acp sur les deux premières colonnes pour éviter de supprimer trop d'éléments
x.acp2 <- x.acp$scores[,1:2]
p <- deleteOutliers(x.acp2)
#on a 2 colonnes et 3497 éléments


#graphe de la perte d'inertie en fonction du nombre de classes
inertie.x.minmut <- rep(0,times=6)
for (k in 1:6){ clus <- kmeans(p[,1:2],centers=k,nstart=100)
inertie.x.minmut[k] <- clus$tot.withinss }
plot(1:6,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="inertie minimale")

#avec les kmeans adaptatives
inertie.x.minmut <- rep(0,times=6)
for (k in 1:6){ clus <- KmeansAdaptative(p[,1:2],numberOfClasses=k,maxIterations=100,precision=0.25, numberOfEssays=10)
inertie.x.minmut[k] <- clus$distanceTotal }
plot(1:6,inertie.x.minmut,type="b",xlab="Nb. de groupes",ylab="distance totale")


class2 <- kmeans(p[,1:2],centers=2,nstart=100)
adjustedRandIndex(p[,3],class2$cluster)


class2ada <- KmeansAdaptative(p[,1:2],numberOfClasses=2,maxIterations=100,precision=0.25, numberOfEssays=10)
adjustedRandIndex(p[,3],class2ada$clusterIndex)