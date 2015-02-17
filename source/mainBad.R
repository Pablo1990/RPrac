#The main program, which runs all the instructions to execute the random forest with cross-validation
#This will work in a bad way, because we do the feature selection before the division between 
#training dataset and testing dataset.
#Arguments:
#nGenes: number of genes involved in the study
#nSubjects: number of subjects involved in the study
#kFold: The number of times we are going to divide the dataset in train and dataset
#selectedGenes: number of genes we are going to select for training dataset
#nTimes: we execute the process n times.
mainBad <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10) {
  #Load the files and libraries needed
  source('source/createDataset.R')
  source('source/filterPvalueBad.R')
  source('source/kfolding.R')
  library('randomForest')
  library("ROCR")
  
  #initialize the counter
  cont <- 0
  #While the counter (i.e. the number of times we have executed the random forest)
  #is lower than the total of times we must execute the random forest.
  iterationOOB <- c()
  while(cont < nTimes) {
    #Creating the dataset and storing in the variable 'datos'
    datos <- createDataset(nGenes, nSubjects)
    #Filtering the set. This is the bad way.
    datos <- filter(datos, selectedGenes, nGenes)
    
    #Store in index.select the vector of orders
    index.select <- kfolding(datos,kFold)
    #Store in err.class the classification errors of randomForest
    err.class <- c()
    
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
      #Store in err.class the classification errors of randomForest
      err.class <- c(err.class,(sum(myrf$confusion[2:3]))/(sum(myrf$confusion[1:4])))
      #After that, we predict what the classifier learned, with the training set.
      mypredict <- predict(myrf, datos$data.test, type="prob")
      mypredict2<-mypredict[,2]

      #Curva Roc
      #roc <- roc(datos$type.test, mypredict2,
       #          # arguments for auc
        #         auc=TRUE,
                 # arguments for ci
         #        ci=TRUE,
                 # arguments for plot
          #       plot=TRUE)
      
      #brierscoreTest<-Brier(myrf, datos$type.test ~ . , data=datos$data.test)
      #print(brierscoreTest)
      #brierscoreTrain<-Brier(myrf, datos$type.train ~ . , data=datos$data.train)
      #print(brierscoreTrain)

      
    }
    #Mean of classification errors of randomForest.
    mean.err.class <- mean(err.class)
    iterationOOB <- c(iterationOOB, mean.err.class)
    
    #Another iteration
    cont <- cont + 1
  }
  return(iterationOOB)
}