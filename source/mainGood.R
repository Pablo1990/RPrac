main <- function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 1) {
  source('source/createDataset.R')
  source('source/filterPvalueGood.R')
  source('source/kfolding.R')
  library('randomForest')
  library("ROCR")
  datos <- createDataset(nGenes, nSubjects)
  #print(datos$pvalues)
  cont <- 0
  while(cont < nTimes) {
    #Divide in testing and training genes
    
    index.select <- kfolding(datos,kFold)
    for(sample.number in 1:kFold) {
      datos$type.train <- datos$type[index.select != sample.number]
      datos$data.train <- datos$data[index.select != sample.number,]
      datos$data.train <- filter(datos,selectedGenes,nGenes)
      
      datos$type.test <- datos$type[index.select == sample.number]
      datos$data.test <- datos$data[index.select == sample.number,]
      #print(datos$data.test)
      #print("-------------------------------")
      #print(datos$data.train)
      #model <- randomForest(genes, type, mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE)
      #tunerf<-tuneRF(x = datos$data.train, y = datos$type.train, ntreeTry=200, stepFactor = 1.5)
      
      myrf <- randomForest(x=datos$data.train, y=datos$type.train, mtry=, ntree=500, keep.forest=TRUE, importance=TRUE)
      mypredict <- predict(myrf, datos$data.test)
      mypredict <- predict(myrf, datos$data.test, type = "prob")#Aquí tenemos las probabilidades de cada prediccion.
     
      #score de brier, curva Roc
      library("ModelGood")
      prob<- predictStatusProb(myrf, datos$data.test)#Una lista del riesgo predictivo.
      #Roc(object, formula, data, splitMethod='noSplitMethod'), esto sacaría tb el score de brier, área bajo la curva etc
      Roc<-Roc(prob~data$data.test, data = datos$data, splitMethod = bootCV)#da error
      #plot(Roc)
      plot(Roc, ylab = "Sensitivity", xlab = "1-Specificity", models,
           type = "l", shadow = FALSE, simu = FALSE, control, grid = FALSE,
           diag = TRUE, box = FALSE, lwd = 2, lty, col, add = FALSE,
           axes = TRUE, legend, auc, percent = TRUE, ...)
      
      #La otra forma con libraryROCR
      
      mypredict2<-mypredict[,1]#he cogido solo las de estar afectado. 
      #mypredict3<-mypredict[,2]
      #CURVA ROC
      #pred <- prediction(predictions, labels)
      pred <- prediction(mypredict2, datos$type.test)#solo funciona con el type...está mal, y la curva que sale tb..digo yo.
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
      
    }
    
    cont <- cont + 1
  }
  
  #print(length(myrfList))
}