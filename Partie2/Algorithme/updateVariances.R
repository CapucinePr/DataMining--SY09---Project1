updateVariances<- function(centers, partition, variances){
  for (k in 1:nrow(centers)){    
    p =ncol(centers)
    nk = lengths(partition)[k]
    
    variance_k = (t(partition[[k]] - centers[k,]) %*% (partition[[k]] - centers[k,])) / nk
    variances[,,k] = det(variance_k)^(-1/p) * variance_k
  }
  return(variances)
}