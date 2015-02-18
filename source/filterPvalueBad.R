#Feature selection. It select most important genes according to the pvalue
#Arguments:
#data: the whole dataset
#selectedGenes: number of genes we are going to select for training dataset
#nGenes: number of genes involved in the study
#return the filtered data
filter <- function (datos,selectedGenes,nGenes) {
  #Get the N selectedGenes of the training set according to the lowest pvalues.
  filtered <- datos$pvalues <= quantile (datos$pvalues,(selectedGenes/nGenes))
  
  #Do the filtered to the genes
  positions <- which(!filtered)
  data <- datos$data[,-positions]
  
  #And the types too
  type <- datos$type
  pvalues <- datos$pvalues[-positions]
  
  #Return the filtered data
  return (list(data = data, pvalues = pvalues, type = type))
}

