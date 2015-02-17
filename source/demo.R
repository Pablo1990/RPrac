source('source/mainGood.R')
source('source/mainBad.R')

OOBbad <- mainBad()
OOBgood <- mainGood()

prueba <- data.frame (OOB= c(OOBgood,OOBbad), 
                      method = rep(c("Good","Bad"),c(length(OOBgood),length(OOBbad)))
)
