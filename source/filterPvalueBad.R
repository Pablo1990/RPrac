#Feature selection
#Arguments:
#datos: the whole dataset
#selectedGenes: number of genes we are going to select for training dataset
#nGenes: number of genes involved in the study
filter <- function (datos,selectedGenes,nGenes) {
  
  filtered <- datos$pvalues <= quantile (datos$pvalues,(selectedGenes/nGenes))
  
  positions <- which(!filtered)
  
  data <- datos$data[,-positions]
  
  type <- datos$type
  
  pvalues <- datos$pvalues[-positions]
  
  return (list(data = data, pvalues = pvalues, type = type))
}

