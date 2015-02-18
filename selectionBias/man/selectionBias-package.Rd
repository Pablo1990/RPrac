\name{selectionBias-package}
\alias{selectionBias-package}
\alias{selectionBias}
\docType{package}
\title{
Showing the Selection Bias problem.
}

\description{
We create two main programs, in which simulate the selection bias problem with the random forest algorithm. 
In the mainGood we do it in a right way, doing the feature selection within the kfolding. In the second one, we do it in a wrong way, doing the feature selection before the kfolding.
MSc Bioinformatics 2014-2015 - R & statistics exercise
Presentation - https://docs.zoho.com/show/publish/pnmm5185d5e8a3ce8459e9903c0954bdfb2d9
}

\details{
\tabular{ll}{
Package: \tab selectionBias\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-18\cr
License: \tab No license\cr
}

}

\author{
Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero

Maintainer: Jose Alejandro Romero: joseale2310@hotmail.es

}
\keyword{ package }
\keyword{ selection bias }


\examples{
op <- par(mfrow = c(1,2))

require(car)

bad10 <- mainBad(nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10)
good10 <- mainGood(nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 10)

bad50 <- mainBad(nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 50)
good50 <- mainGood(nGenes = 1000, nSubjects = 50, kFold = 10, selectedGenes = 50)



sample <- data.frame (OOB = c(good$OOB,bad$OOB), 
                      brier = c(good$brier,bad$brier),
                      method = rep(c("Good","Bad"),c(length(good$OOB),length(bad$OOB)))
)

Boxplot( OOB ~ method, data = sample)
Boxplot( brier ~ method, data = sample)

par(op)

}
