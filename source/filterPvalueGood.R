filter <- function (data,selectedGenes,nGenes) {
  
  filtered <- data$pvalues <= quantile (data$pvalues,(selectedGenes/nGenes))
  
  positions <- which(!filtered)
  
  datafiltered <- data$[,-positions]
  
  return (datafiltered)
}
