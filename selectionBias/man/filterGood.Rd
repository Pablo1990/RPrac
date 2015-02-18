\name{filterGood}
\alias{filterGood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filterGood(data, selectedGenes, nGenes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{selectedGenes}{
%%     ~~Describe \code{selectedGenes} here~~
}
  \item{nGenes}{
%%     ~~Describe \code{nGenes} here~~
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
function (data, selectedGenes, nGenes) 
{
    train.pvalues <- apply(data$data.train[1:nrow(data$data.train), 
        1:ncol(data$data.train)], 2, function(x) t.test(x ~ data$type.train)$p.value)
    filtered <- train.pvalues <= quantile(train.pvalues, (selectedGenes/nGenes))
    positions <- which(!filtered)
    datafiltered <- data$data.train[, -positions]
    return(datafiltered)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
