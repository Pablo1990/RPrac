#The main program, which runs all the instructions to execute the random forest with cross-validation
#This will work in a bad way, because we do the feature selection before the division between 
#training dataset and testing dataset.
#Arguments:
#nGenes: number of genes involved in the study
#nSubjects: number of subjects involved in the study
#kFold: The number of times we are going to divide the dataset in train and dataset
#selectedGenes: number of genes we are going to select for training dataset
#nTimes: we execute the process n times.
mainBad <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 1) {
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