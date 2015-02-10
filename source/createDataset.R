#Create dataset, but depending on the arguments: 
# number of genes, number of subjects
createDataset <- function(nGenes = 100, nSubjects = 50){
  #￼help(rnorm) help(runif) help(rpois)
  #set.seed(2) #for testing
  
  #type <- factor(rep(c("NONAFFECTED","AFFECTED"),length.out = nSubjects))
  #genes <- matrix(rnorm(nGenes*nSubjects), nrow = nSubjects) #page 69 R-Bioinfo-intro
  
  data <- data.frame (gene = matrix(rnorm(nGenes*nSubjects), nrow = nSubjects)) #page 69 R-Bioinfo-intro
  
  type <- factor(rep(c("NONAFFECTED","AFFECTED"),length.out = nSubjects))
  
  #pvalues <- apply(genes, 2,
  #                 function(x) t.test(x ~ type)$p.value)
  pvalues <- apply(data[1:nrow(data),1:ncol(data)], 2, 
                   function(x) t.test(x ~ type)$p.value)
  #(hist(pvalues))
  #pvalues
  return (list(data = data, pvalues = pvalues, type = type))
  #attach(createDataset(1000, 50))
}