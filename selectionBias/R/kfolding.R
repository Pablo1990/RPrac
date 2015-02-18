#Create a vector which says the position the order of the testing positions.
#Arguments:
#datos: the data we are going to divide
#kfold: number of divisions of the testing
#return an index with the positions
kfolding <- function (x,kfold=10){
  #Get the total amount of rows we have
  N <- nrow(x$data)
  
  #Create the vector of the relative positions of the testing subjects
  index.select <- sample(rep(1:kfold, length = N), N, replace = FALSE)
  
  #return the index of the testing
  return(index.select = index.select)
}
