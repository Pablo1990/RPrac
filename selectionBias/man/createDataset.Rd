\name{createDataset}
\alias{createDataset}

\title{
Create data set
}
\description{
createDataset creates random data necessary for the bias selection problem.
}
\usage{
createDataset(nGenes = 100, nSubjects = 50)
}
\arguments{
  \item{nGenes}{
A positive integer which indicates the number of genes to be created.
}
  \item{nSubjects}{
A positive integer which indicates the number of subjects to be created.
}
}
\details{
The more the number of genes and subjects, the more time will take to compute the results of the following functions. Nevertheless, it is needed a mininum number of genes and subjects in order to use the whole package, but it depends of the k-fold number and number of selected genes for the feature selection.
}
\value{
Returns three objects:
\item{data}{A data frame of random values for every gene and subject.}
\item{type}{A factor vector of two levels (AFFECTED and NONAFFECTED) as long as the number of subjects.}
\item{pvalues}{A vector p-values of a t-test of the genes.}
}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero
}

\examples{
set.seed(1)
# Creates an example of data set.
x <- createDataset(1000,50)
# See data set.
print(x)
}
\keyword{ create }
\keyword{ data set }
\keyword{ random }