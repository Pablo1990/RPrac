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
  #Load the files and libraries needed
  source('source/createDataset.R')
  source('source/filterPvalueBad.R')
  source('source/kfolding.R')
  library('randomForest')
  library("ROCR")
  
  #Creating the dataset and storing in the variable 'datos'
  datos <- createDataset(nGenes, nSubjects)
  #Filtering the set. This is the bad way.
  datos <- filter(datos, selectedGenes, nGenes)
  
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
      datos$data.train <- datos$data[index.select != sample.number,]
      datos$data.test <- datos$data[index.select == sample.number,]
      datos$type.train <- datos$type[index.select != sample.number]
      datos$type.test <- datos$type[index.select == sample.number]
      
      #Execute the random forest, y would the labels "affected" and "NonAffected"
      #and x the training set
      myrf <- randomForest(y=datos$type.train, x=datos$data.train,mtry=2, ntree=500, keep.forest=TRUE, importance=TRUE)
      #After that, we predict what the classifier learned, with the training set.
      mypredict <- predict(myrf, datos$data.test, type="prob")
      mypredict2<-mypredict[,1]
      
      #ROC
      #pred <- prediction(predictions, labels)
      pred <- prediction(mypredict2, datos$type.test)
      perf <- performance(pred, measure = "tpr", x.measure = "fpr")
      plot(perf, col=rainbow(10))
      #(rfcv(datos$data.test, datos$type, cv.fold=kFold)) #esto es algo que hace crossvalidation
      #pero no entiendo nada de lo que hace
      
      #print(mypredict)
      #print("-------------------")
      #print(myrf$predicted)
      #print(myrf)
      #print(myrf$predicted)
      #print(myrf$confusion)
      brierscoreTrain<-Brier(myrf, datos$type.test ~ . , data=datos$data.test)
      print(brierscoreTrain)
      brierscoreTest<-Brier(myrf, datos$type.train ~ . , data=datos$data.train)
      print(brierscoreTest)
      
      #plot(Roc)
      #Roc<-plot(Roc, ylab = "Sensitivity", xlab = "1-Specificity", models,
       #    type = "l", shadow = FALSE, simu = FALSE, control, grid = FALSE,
        #   diag = TRUE, box = FALSE, lwd = 2, lty, col, add = FALSE,
         #  axes = TRUE, legend, auc, percent = TRUE, ...)
      
    }
    
    #Another round of the random forest
    cont <- cont + 1
  }
}