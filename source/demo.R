source('source/mainGood.R')
source('source/mainBad.R')

OOBbad <- mainBad(1000,50,10,50)$OOB
OOBgood <- mainGood(1000,50,10,50)$OOB

prueba <- data.frame (OOB= c(OOBgood,OOBbad), 
                      method = rep(c("Good","Bad"),c(length(OOBgood),length(OOBbad)))
)
