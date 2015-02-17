source('source/mainGood.R')
source('source/mainBad.R')
require(car)

bad <- mainBad(1000,50,10,50)
good <- mainGood(1000,50,10,50)

prueba <- data.frame (OOB= c(good$OOB,bad$OOB), 
                      brier = c(good$brier,bad$brier),
                      method = rep(c("Good","Bad"),c(length(OOBgood),length(OOBbad)))
)

Boxplot( OOB ~ method, data=prueba)
Boxplot( brier ~ method, data=prueba)