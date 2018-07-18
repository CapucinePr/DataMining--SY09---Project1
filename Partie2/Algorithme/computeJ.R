
computeJ <-function(data, variances, centers, partition){
  J = 0
  numberOfCenters = nrow(centers)
  tabJ = rep(0, numberOfCenters)
  for(k in 1:numberOfCenters){
    tabJ[k] = sum(distXY(partition[[k]], centers[k,], solve(variances[,,k])))
  }
  J = sum(tabJ)
  resultat = list("totalDistance" = J, "distanceByCluster"= tabJ)
  return(resultat)
}