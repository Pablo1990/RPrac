filter <- function () {
  filtered <- data$pvalues <= quantile (data$pvalues,0.1)
  
  positions <- which(filtered)
  
  genes <- data$genes[,positions]
  
  type <- data$type[,positions]
  
  pvalues <- data~pvalues[,positions]
  
  return (list(type = type, genes = genes, pvalues = pvalues))
}

