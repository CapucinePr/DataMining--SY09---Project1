setwd("C:/Users/Yann-Maillet/Documents/UTC/GI04/SY09/Projet1/Algo")

devtools::install_github("renkun-ken/rlist")
install.packages("rlist")
library(rlist)
library(MASS)
library(matrixcalc)

source("distXY.R")
source("initializeCenters.R")
source("initializeVariances.R")
source("clusterPoints.R")
source("updateCenters.R")
source("updateVariances.R")
source("updateConvergence.R")
source("computeJ.R")
source("KEqual1.R")


numberOfEssays = 10
precision =  0.0001
maxIterations = 100
data = matrix(c(1:5000), ncol =2, byrow=TRUE)  # juste pour tester
numberOfClasses = 5



KmeansAdaptative <- function(data, numberOfClasses, maxIterations, precision, numberOfEssays){
  if(numberOfClasses==1){              #on traite le cas k=1 à part
    resultat = KEqual1(data)
    return(resultat)
  }
  else{
    firstLoop = TRUE
    for(i in 1:numberOfEssays){
      numberOfIteration = 0
      convergence = 1                                                           #initialisation des params
      centers = initializeCenters(data, numberOfClasses)           
      variances = initializeVariances(ncol(data), numberOfClasses)
      
      while (!is.nan(convergence) && numberOfIteration < maxIterations && convergence > precision ){
        partition = clusterPoints(data, centers, variances)                        # calcul de l'optimum local
        precedentCenters = centers
        centers = updateCenters(data, partition$partition, centers)
        variances = updateVariances(centers, partition$partition, variances)
        convergence = updateConvergence(precedentCenters, centers, variances)
        numberOfIteration = numberOfIteration +1
      }
      if(!is.nan(convergence)){
        if(firstLoop == TRUE){ 
          bestJ = computeJ(data,variances, centers, partition$partition)                                    # on regarde si l'optimum local est mieux que le precedent
          resultat = list("centers" =  centers, "clusterIndex"= partition$partitionIndex,  "cluster"= partition$partition, "distanceByCluster"=bestJ$distanceByCluster, "distanceTotal" = bestJ$totalDistance)
          firstLoop = FALSE
         }
        else{
          J = computeJ(data,variances, centers, partition$partition)        
          if(J$totalDistance < bestJ$totalDistance){
            bestJ = J
            resultat = list("centers" =  centers, "clusterIndex"= partition$partitionIndex,  "cluster"= partition$partition, "distanceByCluster"=bestJ$distanceByCluster, "distanceTotal" = bestJ$totalDistance)
          }
        }
      }
    }
    return(resultat)
  }
}


KmeansAdaptative(data, numberOfClasses, maxIterations, precision, numberOfEssays)
