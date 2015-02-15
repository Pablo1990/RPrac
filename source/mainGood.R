#The main program, which runs all the instructions to execute the random forest with cross-validation
#This will work in a good way
#Arguments:
#nGenes: number of genes involved in the study
#nSubjects: number of subjects involved in the study
#kFold: The number of times we are going to divide the dataset in train and dataset
#selectedGenes: number of genes we are going to select for training dataset
#nTimes: we execute the process n times.
mainGood <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 1) {
  
  #Header
  #Load the files and libraries needed
  source('source/createDataset.R')
  source('source/filterPvalueGood.R')
  source('source/kfolding.R')
  library('randomForest')
  library("ROCR")
  
  #---------------------
  
  #Creating the dataset and storing in the variable 'datos'
  datos <- createDataset(nGenes, nSubjects)
  
  #initialize the counter
  cont <- 0
  #While the counter (i.e. the number of times we have executed the random forest)
  #is lower than the total of times we must execute the random forest.
  while(cont < nTimes) {
    #Store in index.select the vector of orders
    index.select <- kfolding(datos,kFold)
    
    #Doing the kfolding
    for(sample.number in 1:kFold) {
      #Divide in testing and training genes
      #If the position is in this round of the folding and is the same
      #in the index.select, it will go to the testing set, otherwise
      #to the training set.
      datos$type.train <- datos$type[index.select != sample.number]
      datos$data.train <- datos$data[index.select != sample.number,]
      datos$data.train <- filter(datos,selectedGenes,nGenes)
      
      #Testing set
      datos$type.test <- datos$type[index.select == sample.number]
      datos$data.test <- datos$data[index.select == sample.number,]
      #Execute the random forest, y would the labels "affected" and "NonAffected"
      #and x the training set      
      myrf <- randomForest(x=datos$data.train, y=datos$type.train, mtry=2, ntree=500, keep.forest=TRUE, importance=TRUE)
      #Predicting what the classifier learned, with the training set.
      mypredict <- predict(myrf, datos$data.test, type = "prob")
      mypredict2<-mypredict[,1]
      #mypredict3<-mypredict[,2]
      
      #ROC
      #pred <- prediction(predictions, labels)
      pred <- prediction(mypredict2, datos$type.test)#solo funciona con el type...está mal, y la curva que sale tb..digo yo.
      perf <- performance(pred, measure = "tpr", x.measure = "fpr")
      plot(perf, col=rainbow(10))
      
    }
    #Another round of the random forest
    cont <- cont + 1
  }
}