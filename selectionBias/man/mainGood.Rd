\name{mainGood}
\alias{mainGood}
\title{
Right method to train a random forest classifier
}
\description{
The main program, which runs all the instructions to execute the random forest with cross-validation.
This will work in the right way, because we do the feature selection within the division between 
training dataset and testing dataset, over-estimating the strength of the classifier.
}
\usage{
mainGood(nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10)
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
There are a few details you may want to know, when you change the default values. If you increased the number of genes of the dataset, the training will learn more, and (of course) the training would be better. That's because of the random data, if it is not, just be careful with overfitting. Another important thing is, when you select more genes with the 'selectedGenes' parameter, there would be more difference between the mainGood and mainBad.
}
\value{

\item{OOB}{A vector of size 'ntimes', in which you can see in every position the training error mean of each iteration of the random forest}
\item{brier}{A vector of size 'ntimes', in which you can see in every position the brier score mean of each iteration of the random forest}

}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero
}
\note{
Try different values in the parameters: 'selectedGenes', 'nGenes' and 'nSubjects in order to see how the brier score and training error increased or decreased in both mains.
}

\examples{
require(car)
sample <- mainGood()
op <- par(mfrow = c(2,2))
Boxplot (sample$OOB)
Boxplot (sample$brier)
par(op)
}

\keyword{ selection }
\keyword{ bias }
\keyword{ right }
