kfolding <- function (datos){
  N <- length(datos$data[,-ncol(datos$data)])
  kFold <- 10 ## el k-fold
  ## en la k-vez, dejamos en testing set
  ## los que tienen index.select = k
  ## O sea, index.select es el vector de índices
  index.select <- sample(rep(1:kFold,
                             length = N),
                         N, replace = FALSE)
  return(index.select = index.select)
}
