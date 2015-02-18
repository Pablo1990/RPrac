\name{filterBad}
\alias{filterBad}
\title{
Feature selection by the wrong method
}
\description{
It selects the genes with the lower p-values and the number of genes selected.
} 
\usage{
filterBad(data, selectedGenes, nGenes)
}

\arguments{
  \item{data}{
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
It returns the list of objects created in the createDataset function, filtered by the p-value. It must be used before the k-fold loop.
}
\value{
\item{data}{A filtered data frame of random values for every gene and subject.}
\item{type}{A filtered factor vector of two levels (AFFECTED and NONAFFECTED) as long as the number of subjects.}
\item{pvalues}{A filtered vector p-values of a t-test of the genes.}
}
}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero
}

\examples{

set.seed(1)
x <- createDataset(1000,50)
y <- filterBad(x,50,1000)

print(x)
print(y)
print("The difference between x and y genes is:")
print (ncol(x$data)-ncol(x$data))

}

