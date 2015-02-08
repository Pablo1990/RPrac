main <- function (nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  "hola hermosos"
  source('source/createDataset.R')
  source('source/filterPvalue.R')
  source('source/crossvalidation.R')
  data <- createDataset(nGenes, nSubjects)
  #fs <- filter(data)
  cont <- 0
  while(cont < nTimes) {

    #Divide in testing and training genes
   
    index.select <- kfolding(data)
    for(sample.number in 1:kFold) {
      data$genes.train <- data$genes[index.select != sample.number]
      data$genes.test <- data$genes[index.select == sample.number]
      print(data$genes.train)
      print("-------------------------------")
    }
    #type
    #genes
    #pvalues
    #Filter with the p-value the training set
    #Execute randomForest
    randomForest(genes, type)
      
    cont <- cont + 1
  }
}