kfolding <- function (datos,kFold){
  N <- nrow(datos$data)
  kfold <- kFold ## el k-fold
  ## en la k-vez, dejamos en testing set
  ## los que tienen index.select = k
  ## O sea, index.select es el vector de índices
  index.select <- sample(rep(1:kfold,
                             length = N),
                         N, replace = FALSE)
  return(index.select = index.select)
}
