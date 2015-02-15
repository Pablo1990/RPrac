#data frame
createDataset <- function(nGenes = 100, nSubjects = 50){
   
  data <- data.frame (gene = matrix(rnorm(nGenes*nSubjects), nrow = nSubjects), #page 69 R-Bioinfo-intro
                      type = factor(rep(c("NONAFFECTED","AFFECTED"),length.out = nSubjects))
                      
  )
  
  return (data)

}

pvalues <- apply(data[1:nrow(data),1:ncol(data)], 2, function(x) t.test(x ~ data$type)$p.value)

#id = replicate(23, paste(sample(letters, 10), collapse = "")))