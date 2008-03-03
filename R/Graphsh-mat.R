
# adjmat <- function(gg, vn=nodes(gg)){
#   imat  <- matrix(FALSE, nc=length(vn), nr=length(vn))
#   dimnames(imat) <- list(vn,vn)
#   ed    <- edges(gg)

#   for (i in 1:length(ed)){
#     curre <- ed[[i]]
#     imat[curre[2],curre[1]]<- TRUE
#   }
#   if (inherits(gg, "ugsh")){
#     imat <- imat + t(imat) 
#     storage.mode(imat) <- 'logical'
#   }
#   return(imat)
# }

## Convert undirect graph to adjacency matrix
##
as.adjmat <- function(gg, vn=nodes(gg)){
  imat  <- matrix(FALSE, nc=length(vn), nr=length(vn))
  dimnames(imat) <- list(vn,vn)
  ed    <- edges(gg)

  for (i in 1:length(ed)){
    curre <- ed[[i]]
    imat[curre[2],curre[1]]<- 1
  }
  if (inherits(gg, "ugsh")){
    imat <- imat + t(imat)
  }
  return(imat)
}


## Convert adjacency matrix to undirected graph
##
as.graphsh <- function(amat){
  is.ug <-  identical(t(amat),amat)
  vn    <- colnames(amat)

  if (is.ug){
    amat  <- upper.tri(amat) & amat
    ii    <- which(amat, arr.ind=TRUE)
    amat  <- matrix(vn[ii],nc=2)
    new("ugsh", nodes=vn, edges=split(amat, row(amat)))
  } else {
    amat  <- t(amat)
    ii    <- which(amat, arr.ind=TRUE)
    amat  <- matrix(vn[ii],nc=2)
    new("dagsh", nodes=vn, edges=split(amat, row(amat)))
  }
}



