#Create dataset, but depending on the arguments: 
# number of genes, number of subjects
createDataset <- function(nGenes = 100, nSubjects = 50){
  #￼help(rnorm) help(runif) help(rpois)
  #set.seed(2) #for testing
  if(nSubjects%2==1){
    
  }
  type <- factor(c(rep("NONAFFECTED", nSubjects/2), rep("AFFECTED", nSubjects/2)))
  
  genes <- matrix(rnorm(nGenes*nSubjects), nrow = nSubjects) #page 69 R-Bioinfo-intro
  
  pvalues <- apply(genes, 2,
                   function(x) t.test(x ~ type)$p.value)
  #(hist(pvalues))
  #pvalues
  return (list(type = type, genes = genes, pvalues = pvalues))
  #attach(createDataset(1000, 50))
}