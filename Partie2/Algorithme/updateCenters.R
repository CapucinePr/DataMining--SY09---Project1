
updateCenters <- function(data, partition, centers){
  for (k in 1:nrow(centers)){            
    nk = lengths(partition)[k]
                                          
    if(nk==1){                            
      centers[k,] = partition[k]
    }
    else{
      centers[k,] = lapply(partition[k], colSums)[[1]]/nk    # on somme pour trouver le nouveau centre
    }
  }
  return(centers)
}