#Feature selection. It selects the most important genes according to the pvalue
#Arguments:
#data: the whole dataset
#selectedGenes: number of genes we are going to select for training dataset
#nGenes: number of genes involved in the study
#return the filtered data
filterBad <- function (x,selectedGenes,nGenes) {
  #Get the N selectedGenes of the training set according to the lowest pvalues.
  filtered <- x$pvalues <= quantile (datos$pvalues,(selectedGenes/nGenes))
  
  #Do the filtered to the genes
  positions <- which(!filtered)
  data <- x$data[,-positions]
  
  #And the types too
  type <- x$type
  pvalues <- x$pvalues[-positions]
  
  #Return the filtered data
  return (list(data = data, pvalues = pvalues, type = type))
}

