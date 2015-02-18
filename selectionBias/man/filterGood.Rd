\name{filterGood}
\alias{filterGood}

\title{
Feature selection by the right method
}

\description{
It selects the genes with the lower p-values in the train data set.
}

\usage{
filterGood(data, selectedGenes, nGenes)
}

\arguments{
  \item{x}{
Data frame of genes in columns and subjects in rows.
}
  \item{selectedGenes}{
Number of genes with the lower p-values.
}
  \item{nGenes}{
Number of genes within the data frame.
}
}

\details{
It returns the train data set filtered by p-values.
}
\value{

\item{datafiltered}{A filtered data frame of random values for every gene.}

}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero

}

\examples{
  source('source/createDataset.R')
  source('source/filterPvalueGood.R')
  source('source/kfolding.R')
  # Create a data set for the example
  x <- createDataset(1000,50)
  # Create an index fot the k-fold
  index.select <- kfolding(x,10)
  # K-fold loop
  for(sample.number in 1:10) {
      #Divide in testing and training genes
      #If the position is in this iteration of the folding and is the same
      #in the index.select, it will go to the testing set, otherwise
      #to the training set.
      x$type.train <- datos$type[index.select != sample.number]
      x$data.train <- datos$data[index.select != sample.number,]
      #Get the genes with more important pvalues
      x$data.train <- filterGood(datos,selectedGenes,nGenes)
      print(x$data)
      print(x$data.train)
      }
}

\keyword{ filter }
\keyword{ p-value }
\keyword{ good }
