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
Number of genes with the lower p-values.
}
  \item{nTimes}{
Number of times that will be executed the cross-validation in order to test the variability of the classifier.
}
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, 
    nTimes = 10) 
{
    source("source/createDataset.R")
    source("source/filterPvalueBad.R")
    source("source/kfolding.R")
    library("randomForest")
    library("ROCR")
    cont <- 0
    iterationOOB <- c()
    iterationbrier <- c()
    while (cont < nTimes) {
        datos <- createDataset(nGenes, nSubjects)
        datos <- filter(datos, selectedGenes, nGenes)
        index.select <- kfolding(datos, kFold)
        err.class <- c()
        brierscore <- c()
        for (sample.number in 1:kFold) {
            datos$data.train <- datos$data[index.select != sample.number, 
                ]
            datos$data.test <- datos$data[index.select == sample.number, 
                ]
            datos$type.train <- datos$type[index.select != sample.number]
            datos$type.test <- datos$type[index.select == sample.number]
            myrf <- randomForest(y = datos$type.train, x = datos$data.train, 
                mtry = 2, ntree = 500, keep.forest = TRUE, importance = TRUE)
            prediction <- predict(myrf, datos$data.test)
            confusion.table <- table(factor(datos$type.test), 
                prediction)
            err.class <- c(err.class, (1 - sum(diag(confusion.table))/(sum(confusion.table))))
            mypredictprob <- predict(myrf, datos$data.test, type = "prob")
            mypredictresp <- predict(myrf, datos$data.test, type = "response")
            mypredict2 <- mypredictprob[, 2]
            bscore <- brierscore(mypredictresp ~ mypredictprob, 
                data = datos$type.test)
            brierscore <- c(brierscore, (mean(bscore)))
        }
        mean.err.class <- mean(err.class)
        brierscoremean <- mean(brierscore)
        iterationOOB <- c(iterationOOB, mean.err.class)
        iterationbrier <- c(iterationbrier, brierscoremean)
        cont <- cont + 1
    }
    return(list(OOB = iterationOOB, brier = iterationbrier))
  }
}

\keyword{ selection bias }
\keyword{ wrong }
