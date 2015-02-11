filter <- function (data,selectedGenes,nGenes) {
  
  train.pvalues <- apply(data$data.train[1:nrow(data$data.train),1:ncol(data$data.train)], 2, 
                   function(x) t.test(x ~ data$type.train)$p.value)
  
  filtered <- train.pvalues <= quantile (train.pvalues,(selectedGenes/nGenes))
  
  positions <- which(!filtered)
  
  datafiltered <- data$data.train[,-positions]
  
  return (datafiltered)
}
