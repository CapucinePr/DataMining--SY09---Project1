initializeVariances<- function(p, numberOfClasses){
  variances = array(diag(p), dim = c(p,p,numberOfClasses))   #on peut prendre diag(p) car on prend ρ (ro) = 1
  return(variances)                                          # donc ρ^(-1/p) * Ip = Ip
}
