main <- function (nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  "hola hermosos"
  source('source/createDataset.R')
  createDataset(nGenes, nSubjects)
  #Divide in testing and training genes
  #type
  #genes
  #pvalues
  #Filter with the p-value the training set
  testing = sample(nGenes, selectedGenes)
  testing
  nGenes
  #Execute randomForest
}