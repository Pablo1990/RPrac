filter <- function (datos,selectedGenes,nGenes) {
  filtered <- datos$pvalues <= quantile (datos$pvalues,(selectedGenes/nGenes))
  
  positions <- which(!filtered)
  
  data <- datos$data[,-positions]
  
  type <- datos$type
  
  pvalues <- datos$pvalues[-positions]
  
  return (list(data = data, pvalues = pvalues, type = type))
}

