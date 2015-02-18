#Create dataset, but depending on the arguments: 
#nGenes: number of genes involved in the study
#nSubjects: number of subjects involved in the study
#return the created dataset
createDataset <- function(nGenes = 100, nSubjects = 50){
  #￼help(rnorm) help(runif) help(rpois)
  #set.seed(2) #for testing
  
  #Create a data frame with genes in columns, and in rows the subjects.
  data <- data.frame (gene = matrix(rnorm(nGenes*nSubjects), nrow = nSubjects))
  
  #Generating the types of the subjects divide in affected and nonAffected
  type <- factor(rep(c("NONAFFECTED","AFFECTED"),length.out = nSubjects))

  
  #Calculate the pvalues of every gene
  pvalues <- apply(data, 2, 
                   function(x) t.test(x ~ type)$p.value)
  
  #Return every variable calculated
  return (list(data = data, pvalues = pvalues, type = type))
}