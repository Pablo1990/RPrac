\name{mainBad}
\alias{mainBad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mainBad(nGenes = 100, nSubjects = 50, kFold = 10, selectedGenes = 10, nTimes = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nGenes}{
%%     ~~Describe \code{nGenes} here~~
}
  \item{nSubjects}{
%%     ~~Describe \code{nSubjects} here~~
}
  \item{kFold}{
%%     ~~Describe \code{kFold} here~~
}
  \item{selectedGenes}{
%%     ~~Describe \code{selectedGenes} here~~
}
  \item{nTimes}{
%%     ~~Describe \code{nTimes} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
