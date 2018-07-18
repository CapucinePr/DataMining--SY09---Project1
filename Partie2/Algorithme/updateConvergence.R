updateConvergence <- function(precedentCenters, actualCenters, variances){
  delta = rep(0, nrow(actualCenters))
  for (k in 1:nrow(actualCenters)){
    delta[k] = t(actualCenters[k,] - precedentCenters[k,]) %*% variances[,,k] %*% (actualCenters[k,] - precedentCenters[k,])
  }
  delta = sum(delta)
  return(delta)
}