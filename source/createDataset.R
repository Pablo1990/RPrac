#Create dataset, but depending on the arguments: 
# number of genes, number of subjects
createDataset <- function(nGenes = 100, nSubjects = 50){
  #￼help(rnorm) help(runif) help(rpois)
  #set.seed(2) #for testing
  
  type <- factor(rep(c("NONAFFECTED","AFFECTED"),length.out = nSubjects))
  
  genes <- matrix(rnorm(nGenes*nSubjects), nrow = nSubjects) #page 69 R-Bioinfo-intro
  
  pvalues <- apply(genes, 2,
                   function(x) t.test(x ~ type)$p.value)
  #(hist(pvalues))
  #pvalues
  return (list(type = type, genes = genes, pvalues = pvalues))
  #attach(createDataset(1000, 50))
}