#The main program, which runs all the instructions to execute the random forest with cross-validation
#This will work in a good way
#Arguments:
#nGenes: number of genes involved in the study
#nSubjects: number of subjects involved in the study
#kFold: The number of times we are going to divide the dataset in train and dataset
#selectedGenes: number of genes we are going to select for training dataset
#nTimes: we execute the process n times.
mainGood <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  #Header
  #Load the files and libraries needed 
  source('source/createDataset.R')
  source('source/filterPvalueGood.R')
  source('source/kfolding.R')
  library("ModelGood")
  library('randomForest')
  library("ROCR")
  #---------------------
 
  #initialize the counter
  cont <- 0
  #While the counter (i.e. the number of times we have executed the random forest)
  #is lower than the total of times we must execute the random forest.
  while(cont < nTimes) {
    #Creating the dataset and storing in the variable 'datos'
    datos <- createDataset(nGenes, nSubjects)
    
    #Store in index.select the vector of orders
    index.select <- kfolding(datos,kFold)
    err.class <- c()
    #Doing the kfolding
    for(sample.number in 1:kFold) {
      #Divide in testing and training genes
      #If the position is in this round of the folding and is the same
      #in the index.select, it will go to the testing set, otherwise
      #to the training set.
      datos$type.train <- datos$type[index.select != sample.number]
      datos$data.train <- datos$data[index.select != sample.number,]
      #Get the genes with more important pvalues
      datos$data.train <- filter(datos,selectedGenes,nGenes)
      #Testing set
      datos$type.test <- datos$type[index.select == sample.number]
      datos$data.test <- datos$data[index.select == sample.number,]
      #Execute the random forest, y would the labels "affected" and "NonAffected"
      #and x the training set
      myrf <- randomForest(x=datos$data.train, y=datos$type.train, mtry=2, ntree=500, keep.forest=TRUE, importance=TRUE)
      err.class <- c(err.class,mean(myrf$confusion[,c(3)]))
      #Predicting what the classifier learned, with the training set.
      mypredict <- predict(myrf, datos$data.test, type = "prob")
      mypredict2<-mypredict[,1]
      #mypredict3<-mypredict[,2]
      
      #ROC
      #pred <- prediction(predictions, labels)
      #pred <- prediction(mypredict2, datos$type.test)
      #perf <- performance(pred, measure = "tpr", x.measure = "fpr")
      #plot(perf, col=rainbow(10))
     
      #score de brier. 
      brierscoreTest<-Brier(myrf, datos$type.test ~ . , data=datos$data.test)
      print(brierscoreTest)
      brierscoreTrain<-Brier(myrf, datos$type.train ~ . , data=datos$data.train)
      print(brierscoreTrain)
      #plot(Roc)
      #plot(Roc, ylab = "Sensitivity", xlab = "1-Specificity", models,
       #    type = "l", shadow = FALSE, simu = FALSE, control, grid = FALSE,
        #   diag = TRUE, box = FALSE, lwd = 2, lty, col, add = FALSE,
         #  axes = TRUE, legend, auc, percent = TRUE, ...)
      
    }
    #Mean of classification errors of randomForest
    mean.err.class <- mean(err.class)
    
    #Another iteration
    cont <- cont + 1
  }
}