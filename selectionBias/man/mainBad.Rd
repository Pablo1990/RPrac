\name{mainBad}
\alias{mainBad}

\title{
Wrong method to train a random forest classifier
}
\description{
The main program, which runs all the instructions to execute the random forest with cross-validation.
This will work in a wrong way, because we do the feature selection before the division between 
training dataset and testing dataset, over-estimating the strength of the classifier.
}
\usage{
mainBad(nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10)
}

\arguments{
  \item{nGenes}{
A positive integer which indicates the number of genes to be created.
}
  \item{nSubjects}{
A positive integer which indicates the number of subjects to be created.
}
  \item{kFold}{
Number of randomly partitioned equal size subsamples from the original subject sample.
}
  \item{selectedGenes}{
Number of genes to be selected depending on the lowest p-values.
}
  \item{nTimes}{
Number of times that will be executed the cross-validation in order to test the variability of the classifier.
}
}
\details{
Details
}
\value{
\item{comp1 }{Description of 'comp1'}
\item{comp2 }{Description of 'comp2'}

}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero
}

\examples{
mainBad()
}
\keyword{ selection }
\keyword{ bias }
\keyword{ wrong }
