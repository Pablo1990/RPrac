main <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 1) {
  source('source/createDataset.R')
  source('source/filterPvalueBad.R')
  source('source/kfolding.R')
  library('randomForest')
  library("ROCR")
  datos <- createDataset(nGenes, nSubjects)
  datos <- filter(datos, selectedGenes, nGenes)
  #print(datos$pvalues)
  cont <- 0
  while(cont < nTimes) {
    #Divide in testing and training genes

    index.select <- kfolding(datos,kFold)
    for(sample.number in 1:kFold) {
      datos$data.train <- datos$data[index.select != sample.number,]
      datos$data.test <- datos$data[index.select == sample.number,]
      datos$type.train <- datos$type[index.select != sample.number]
      datos$type.test <- datos$type[index.select == sample.number]
      #print(datos$data.test)
      #print("-------------------------------")
      #print(datos$data.train)
      #model <- randomForest(genes, type, mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE)
      myrf <- randomForest(y=datos$type.train, x=datos$data.train,mtry=2, ntree=500, keep.forest=TRUE, importance=TRUE)
      mypredict <- predict(myrf, datos$data.test)
      
      #(rfcv(datos$data.test, datos$type, cv.fold=kFold)) #esto es algo que hace crossvalidation
      #pero no entiendo nada de lo que hace
      
      #print(mypredict)
      #print("-------------------")
      #print(myrf$predicted)
      #print(myrf)
      #print(myrf$predicted)
      #print(myrf$confusion)
      
    }
    
    cont <- cont + 1
  }
  
  #print(length(myrfList))
}