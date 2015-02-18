\name{kfolding}
\alias{kfolding}
\title{
K-fold index creation
}
\description{
It creates a vector of indexes for the k-fold loop.
}
\usage{
kfolding(x, kfold=10)
}

\arguments{
  \item{x}{
Data frame of genes in columns and subjects in rows.
}
  \item{kfold}{
Number of randomly partitioned equal size subsamples from the original subject sample.
}
}
\details{
In k-fold cross-validation, the original sample is randomly partitioned into k equal size subsamples. Of the k subsamples, a single subsample is retained as the validation data for testing the model, and the remaining k - 1 subsamples are used as training data. The cross-validation process is then repeated k times, the folds, with each of the k subsamples used exactly once as the validation data.
}
\value{

\item{index.select}{A vector of indexes for the k-fold loop}

}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero
}

\examples{
# Create data set for the example

x <- createDataset(1000,50)

# Create indexes
index.select <- kfolding(x,10)

# The index vector will be as long as the number objects, and is used to select which samples will be
# used for the train set, and which one for the test set.
table(index.select)
sum(index.select != 1)
sum(index.select != 10)
sum(index.select != 6)
}
\keyword{ k-fold}
\keyword{ index }
