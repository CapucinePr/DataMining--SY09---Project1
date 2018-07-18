initializeCenters <- function(data, numberOfClasses){
  index = sample(nrow(data), numberOfClasses) 
  centers = data[index, ]  
  return(centers)
}