visualizeData <- function(){
  #hist, plot, scatter3d (library(car)), bloxplot, stripchart, scatterplot
  #Además del error de clasificación, usad los votos o probabilidades que
  #da randomForest cuando predice algo, para obtener el score de Brier y/o
  #el índice de concordancia.
  library("ModelGood")
  library("ROCR")
  #Brier()
  pred <- prediction(predictions, labels)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf, col=rainbow(10))
}