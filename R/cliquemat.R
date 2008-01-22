
## Represent cliques (or other sets) in a matrix...
cliquemat <- function(cli,vn){
  amat <- matrix(0, nr=length(cli), nc = length(vn))
  colnames(amat) <- vn
  
  for (i in 1:length(cli)){
    amat[i, cli[[i]]] <- 1
  }
  amat
}
