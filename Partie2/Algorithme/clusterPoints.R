clusterPoints <- function(data, centers, variances){
  dataRows = nrow(data)
  centersRows = nrow(centers)
  distances = matrix(0, dataRows, centersRows)
  for (k in 1:nrow(centers)){
      distances[,k] = distXY(data, centers[k,], solve(variances[,,k]))
    }

  classIndex=apply(distances,1,which.min)

  partition = list()
  for (k in 1:nrow(centers)) {
    index_k = which(classIndex==k)       # on récupère les index des données de la classe k
    data_k = data[index_k,]           # on récupère maintenant les données correspondant à ces indexs ( une partition donc)
    partition = list.append(partition, data_k)
  }
  
  resultat =  list("partition" = partition, "partitionIndex" = classIndex)
  return(resultat)
}
