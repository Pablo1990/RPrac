main <- function (nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  source('source/createDataset.R')
  source('source/filterPvalue.R')
  source('source/kfolding.R')
  datos <- createDataset(nGenes, nSubjects)
  #fs <- filter(data)
  cont <- 0
  while(cont < nTimes) {

    #Divide in testing and training genes

    index.select <- kfolding(datos)
    for(sample.number in 1:kFold) {
      datos$data.train <- datos$data[index.select != sample.number]
      data$data.test <- datos$data[index.select == sample.number]
      #print(datos$data.test)
      #print("-------------------------------")
      #model <- randomForest(genes, type, mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE)
      model <- randomForest(type ~ ., data=datos$data, mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE)
    }
    #type
    #genes
    #pvalues
    #Filter with the p-value the training set
    #Execute randomForest
    #randomForest(genes, type)
      
    cont <- cont + 1
  }
}