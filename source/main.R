main <- function (nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  "hola hermosos"
  source('source/createDataset.R')
  createDataset(nGenes, nSubjects)
  
  cont <- 0
  while(cont < nTimes) {

    #Divide in testing and training genes
    #type
    #genes
    #pvalues
    #Filter with the p-value the training set
    #Execute randomForest
    randomForest(genes, type)
      
    cont <- cont + 1
  }
}