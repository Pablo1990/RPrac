require(car)
require (RcmdrMisc)

op <- (par(mfrow=c(1,2)))

good10 <- mainGood(nGenes=1000,nSubjects=50,kFold=10,selectedGenes=10)
bad10 <- mainBad(nGenes=1000,nSubjects=50,kFold=10,selectedGenes=10)

good50 <- mainGood(nGenes=1000,nSubjects=50,kFold=10,selectedGenes=50)
bad50 <- mainBad(nGenes=1000,nSubjects=50,kFold=10,selectedGenes=50)

good100g <- mainGood(nGenes=100,nSubjects=50,kFold=10,selectedGenes=50)
bad100g <- mainBad(nGenes=100,nSubjects=50,kFold=10,selectedGenes=50)

good2000g <- mainGood(nGenes=2000,nSubjects=50,kFold=10,selectedGenes=50)
bad2000g <- mainBad(nGenes=2000,nSubjects=50,kFold=10,selectedGenes=50)


sample <- data.frame (OOB10 = c(good10$OOB,bad10$OOB), 
                      brier10 = c(good10$brier,bad10$brier),
                      OOB50 = c(good50$OOB,bad50$OOB),
                      brier50 = c(good50$brier,bad50$brier),
                      OOB100g = c(good100g$OOB,bad100g$OOB),
                      brier100g = c(good100g$brier,bad100g$brier),
                      OOB2000g = c(good2000g$OOB,bad2000g$OOB),
                      brier2000g = c(good2000g$brier,bad2000g$brier),
                      method = rep(c("Good","Bad"),c(length(good10$OOB),length(bad10$OOB)))
)

Boxplot( OOB ~ method, data = sample)
Boxplot( brier ~ method, data = sample)

scatterplotMatrix( ~ OOB10 +OOB50 + OOB100g + OOB2000g | method, 
                   reg.line = FALSE, 
                   smooth = FALSE, 
                   data = sample)

plotMeans(sample$OOB10, sample$method, error.bars = "se", 
          xlab = "method", ylab = "OOB 10 selected genes")

plotMeans(sample$OOB2000g, sample$method, error.bars = "se", 
          xlab = "method", ylab = "OOB 2000 nGenes")



bad50 <- mainBad(nGenes=1000,nSubjects=50,kFold=10,selectedGenes=50)
good50 <- mainGood(nGenes=1000,nSubjects=50,kFold=10,selectedGenes=50)



Boxplot( OOB ~ method, data=sample)
Boxplot( brier ~ method, data=sample)

par(op)

