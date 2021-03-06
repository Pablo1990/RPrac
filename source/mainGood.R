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
  library('randomForest')
  library("pROC")
  library("scoring")
  #---------------------
  iterationOOB <- c()
  iterationbrier<-c()
  #initialize the counter
  cont <- 0
  #While the counter (i.e. the number of times we have executed the random forest)
  #is lower than the total of times we must execute the random forest.
  while(cont < nTimes) {
    #Creating the dataset and storing in the variable 'datos'
    datos <- createDataset(nGenes, nSubjects)
    
    #Store in index.select the vector of orders
    index.select <- kfolding(datos,kFold)
    #Store in err.class the classification errors of randomForest
    err.class <- c()
    brierscore<-c()
    #Doing the kfolding
    for(sample.number in 1:kFold) {
      #Divide in testing and training genes
      #If the position is in this iteration of the folding and is the same
      #in the index.select, it will go to the testing set, otherwise
      #to the training set.
      datos$type.train <- datos$type[index.select != sample.number]
      datos$data.train <- datos$data[index.select != sample.number,]
      #Get the genes with more important pvalues
      datos$data.train <- filter(datos,selectedGenes,nGenes)
      #Testing set
      datos$type.test <- datos$type[index.select == sample.number]
      datos$data.test <- datos$data[index.select == sample.number,]
      
      #Execute the random forest, y = the labels "affected" and "NonAffected"
      #and x=the training set
      myrf <- randomForest(x=datos$data.train, y=datos$type.train, mtry=2, ntree=500, keep.forest=TRUE, importance=TRUE)
      
      #Predicting what the classifier learned with the training set.
      prediction <- predict(myrf,datos$data.test)
      
      #Create the confusion table with the prediction and the real
      confusion.table <- table(factor(datos$type.test),prediction)
      
      #Store in err.class the classification errors of randomForest
      err.class <-  c(err.class,(1-sum(diag(confusion.table))/(sum(confusion.table))))
      
      #Predict with the new data and get the probabilities
      mypredictprob <- predict(myrf, datos$data.test, type = "prob")
      mypredictresp <- predict(myrf, datos$data.test, type = "response")
      mypredict2<-mypredictprob[,2]
      
      #ROC
      #If we get just one class of the existing two, we cannot do the roc curve
      if(length(which(datos$type.test=="AFFECTED"))!=0 && length(which(datos$type.test=="NONAFFECTED"))!=0){
        roc <- roc(response = datos$type.test, predictor = mypredict2,
                   auc=TRUE, ci=TRUE, plot=TRUE)
      } else {
        print ("No ROC this time. There is just one class (affected or nonaffected)")
      }
      
      #Score de brier
      bscore<-brierscore(mypredictresp ~ mypredictprob, data = datos$type.test)

      brierscore<-c(brierscore, (mean(bscore)))
    
    }
    
    #Mean of classification errors of randomForest
    mean.err.class <- mean(err.class)
    brierscoremean<-mean(brierscore)
    iterationOOB <- c(iterationOOB, mean.err.class)
    iterationbrier<-c(iterationbrier, brierscoremean)
    #Another iteration
    cont <- cont + 1
    
  }
  return (list(OOB= iterationOOB, brier = iterationbrier))
  
}