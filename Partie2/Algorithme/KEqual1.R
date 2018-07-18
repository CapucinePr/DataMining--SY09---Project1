KEqual1 <- function(data){
  n = nrow(data)
  p = ncol(data)
  center= colSums(data)/n    # on somme pour trouver le nouveau centre
  distance = distXY(data, center, diag(p))
  J = sum(distance)
  resultat = list("centers" =  center,"distanceTotal" = J)
  return(resultat)
}