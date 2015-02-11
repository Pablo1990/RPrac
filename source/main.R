main <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 1) {
  source('source/createDataset.R')
  source('source/filterPvalue.R')
  source('source/kfolding.R')
  library('randomForest')
  library("ROCR")
  datos <- createDataset(nGenes, nSubjects)
  datos$data <- filter(data, selectedGenes, nGenes)
  cont <- 0
  while(cont < nTimes) {
    #Divide in testing and training genes

    index.select <- kfolding(datos,kFold)
    for(sample.number in 1:kFold) {
      datos$data.train <- datos$data[index.select != sample.number]
      datos$data.test <- datos$data[index.select == sample.number]
      #print(datos$data.test)
      #print("-------------------------------")
      #print(datos$data.train)
      #model <- randomForest(genes, type, mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE)
      myrf <- randomForest(y=datos$type, x=datos$data.train, mtry=2, ntree=500, keep.forest=TRUE, importance=TRUE)
      mypredict <- predict(myrf, datos$data)
      
      #(rfcv(datos$data.test, datos$type, cv.fold=kFold)) #esto es algo que hace crossvalidation
      #pero no entiendo nada de lo que hace
      
      #print(mypredict)
      #print("-------------------")
      #print(myrf$predicted)
      #print(myrf)
      #print(myrf$predicted)
      #print(myrf$confusion)
      break
    }
    
    cont <- cont + 1
  }
  
  #print(length(myrfList))
}