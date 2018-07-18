setwd("C:/Users/Capucine/Documents/UTC/P18/SY09/Projet")
recette <- read.csv("donnees/donnees/recettes-pays.txt")

#1
#only quant variables
recette.quant <- recette[,-1]
#display by origin
plot(recette[,2],recette[,3], col=recette$origin)
plot(recette[,2],recette[,5], col=recette$origin)
#moyenne du nombre d'ingrédients par recette
mean(rowSums(recette.quant>0))
#poids moyen des recettes
mean(rowSums(recette[,-1]))

corRecette <- cor(recette.quant)

#2
#prcomp car plus de variables que d'observations
recette.acp <- prcomp(recette.quant)
plot(recette.acp$x, col=recette$origin)
summary(recette.acp)
#with 5 PC we have 87% of info, 90% with 6
#importance de chaque composante principale
plot(recette.acp)
#technique du coude: on s'arrete entre la deuxième et la troisième CP


#3 - CAH
#matrice de distance
recette.cah <- dist(recette.quant, method="manhattan")

#critere d'agregation ward.D2, prend le carré de la distance
recette.cahWard <- hclust(recette.cah, method="ward.D2")
#show the dendogramme
plot(recette.cahWard)

#dendrogramme avec matérialisation des groupes 
rect.hclust(recette.cahWard,k=3)


#4 - par kmeans
#données centrées
recette.cr <- scale(recette.quant, center=T, scale=F)
#kmeans pour 3 groupes et 5 essais
recette.kmeans <- kmeans(recette.cr,centers=3,nstart=10)
inertie.minmut <- rep(0,times=10)
for (k in 1:10){ clus <- kmeans(recette.cr,centers=k,nstart=100)
inertie.minmut[k] <- clus$tot.withinss }
plot(1:10,inertie.minmut,type="b",xlab="Nb. de groupes",ylab="inertie minimale")
#We have 3 groups of 12, 5 and 9 elements, as with the dendogramme
#repartission des elements:
recette.kmeans$cluster
#group1 : 1 4 5 11 12 14 17 18 19 20 23 24 SAME AS DENDO
#GROUP2: 3 6 15 25 26 SAME AS DENDO
#GROUP3: 2 7 8 9 10 13 16 21 22 SAME AS DENDO

#5 - classification géographique
recette.groupes.cah <- cutree(recette.cahWard,k=3)
plot(recette.acp$x[,1],recette.acp$x[,2])
text(recette.acp$x[,1],recette.acp$x[,2],col=c("red","green","blue")[recette.groupes.cah],labels=recette$origin)
#Pays chauds (afrique, southAm, pays du sud) / Europe, Amerique, Pays tempérés / Asie

#test avec 4 groupes
recette.groupes.cah4 <- cutree(recette.cahWard,k=4)
plot(recette.acp$x[,1],recette.acp$x[,2])
text(recette.acp$x[,1],recette.acp$x[,2],col=c("red","green","blue","yellow")[recette.groupes.cah4],labels=recette$origin)


#6 - recettes_echeant
rec.ech <- read.csv("donnees/donnees/recettes-echant.txt")
#fichier avec 1 col quali (pays d'origine) et des colonnes quanti en binaire (présence ou abscence du produit dans la recette)
corRecEch <- cor(rec.ech[,-1])
#acp
rec.ech.acp <- princomp(rec.ech[,-1])
plot(rec.ech.acp)
rec.ech.acp$loadings
#toutes les CP comptent pas mal, mais surtout les 2-3 premières

# 7
install.packages(pkgs="plyr")
library(plyr)
#fait la moyenne de chaque colonne 
rec.ech.trans <- ddply(rec.ech, .(origin), colwise(mean))
rownames(rec.ech.trans) <- rec.ech.trans$origin
#on enlève l'origine et on fait la transposée pour avoir une rotation du tableau
rec.ech.trans.transpo <- t(rec.ech.trans[,-1])
rec.ech.dist <- dist(rec.ech.trans.transpo, method="manhattan")
rec.ech.trans.ACP <- princomp(rec.ech.dist)
plot(rec.ech.trans.ACP$scores[,1],rec.ech.trans.ACP$scores[,2],col=rec.ech$origin)


#kmeans pour déterminer nb de classes
recette.ech.cr <- scale(rec.ech[,-1], center=T, scale=F)
inertie.ech.minmut <- rep(0,times=8)
for (k in 1:10){ clus <- kmeans(recette.ech.cr,centers=k,nstart=100)
inertie.ech.minmut[k] <- clus$tot.withinss }
plot(1:10,inertie.ech.minmut,type="b",xlab="Nb. de groupes",ylab="inertie minimale")

#8
rec.ech.cahWard <- hclust(rec.ech.dist, method="ward.D2")
plot(rec.ech.cahWard)
#4 classes semblent se distinguer
rect.hclust(rec.ech.cahWard,k=4)

rec.groupes.cah4 <- cutree(rec.ech.cahWard,k=4)
plot(rec.ech.trans.ACP$scores[,1],rec.ech.trans.ACP$scores[,2])
text(rec.ech.trans.ACP$scores[,1],rec.ech.trans.ACP$scores[,2],col=c("red","green","blue","yellow")[rec.groupes.cah4],labels=colnames(rec.ech.trans))

#9
library(cluster)
rec.ech.K <- pam(rec.ech.dist, 4)
rec.ech.K$medoids

#[1] "garlic"       "black_pepper" "milk_fat"    
#[4] "chicken" 

#count the nb of positive values for each row in a matrix
nbNot0 <- function(tab){
  nc <- ncol(tab);
  nr <- nrow(tab);
  l <- vector("list",0);
  for(j in 1:nr){
  n <- 0;
  for(i in 1:nc){
    if(tab[j,i]>0)
      n<-n+1;
  }
   l <- c(l,n); 
  }
  return(l);
}

l<-nbNot0(recette.quant)
mean(unlist(l))






















































