filter <- function () {
  filtered <- data$pvalues <= quantile (data$pvalues,0.1)
  
  positions <- which(filtered)
  
  genes <- data$genes[,positions]
  
  type <- data$type[,positions]
  
  pvalues <- data~pvalues[,positions]
  
  return (list(type = type, genes = genes, pvalues = pvalues))
}


##---------------------------------------------------------------------------------------
## De Venables y Ripley, "S programming", 2000,
##
Springer, p. 175
## x2 es un vector con datos
(x2 <- rnorm(27))
(N <- length(x2))
knumber <- 10 ## el k-fold
## en la k-vez, dejamos en testing set
## los que tienen index.select = k
## O sea, index.select es el vector de índices
(index.select <- sample(rep(1:knumber,
                           length = N),
                       N, replace = FALSE))
rep(1:knumber, length = N)
table(index.select)
sum(index.select != 1)
sum(index.select != 10)
sum(index.select != 6)

##------------------------------------------------------------------------

> x <- rpois(50, 10)
> which(pvalues <= quantile(pvalues, 0.1))
[1]  7 14 16 24 25 28 33 44 45 48
> pvalues[which(pvalues <= quantile(pvalues, 0.1))]
[1] 6 5 4 5 7 7 4 5 6 7
> quantile(x, 0.1) 
