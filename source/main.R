main <- function (nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  "hola hermosos"
  attach(createDataset(nGenes, nSubjects))
  #Divide in testing and training genes
  
  #Filter with the p-value the training set
  
  #Execute randomForest
}