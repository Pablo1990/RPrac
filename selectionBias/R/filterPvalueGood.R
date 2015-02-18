#Feature selection. It select most important genes according to the pvalue
#Arguments:
#data: the whole dataset
#selectedGenes: number of genes we are going to select for training dataset
#nGenes: number of genes involved in the study
#return the filtered data
filterGood <- function (x,selectedGenes,nGenes) {
  #Get the pvalues of the training set
  train.pvalues <- apply(x$data.train, 2, 
                   function(y) t.test(y ~ x$type.train)$p.value)
  
  #Get the N selectedGenes of the training set according to the lowest pvalues.
  filtered <- train.pvalues <= quantile (train.pvalues,(selectedGenes/nGenes))
  
  #Do the filtered
  positions <- which(!filtered)
  datafiltered <- x$data.train[,-positions]
  
  #Return the train filtered
  return (datafiltered)
}
