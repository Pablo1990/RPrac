#Create dataset, but depending on the arguments: 
# number of genes, number of subjects
createDataset <- function(nGenes = 1000, nSubjects = 50){
  #￼help(rnorm) help(runif) help(rpois)
  #set.seed(2) #for testing
  type <- factor(c(rep("Tetudas", nSubjects/2), rep("NoTetudas", nSubjects/2)))
  
  genes <- matrix(rnorm(nSubjects * nGenes), ncol = nSubjects) #page 69 R-Bioinfo-intro 
  
  pvalues <- apply(genes, 1,
                   function(x) t.test(x ~ type)$p.value)
  #(hist(pvalues))
  #pvalues
  return (list(type = type, genes = genes, pvalues = pvalues))
  #attach(createDataset(1000, 50))
}