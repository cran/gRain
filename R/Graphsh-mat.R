
adjmat <- function(gg, vn=nodes(gg)){
  imat  <- matrix(FALSE, nc=length(vn), nr=length(vn))
  dimnames(imat) <- list(vn,vn)
  ed    <- edges(gg)

  for (i in 1:length(ed)){
    curre <- ed[[i]]
    imat[curre[2],curre[1]]<- TRUE
  }
  if (inherits(gg, "ugsh")){
    imat <- imat + t(imat) 
    storage.mode(imat) <- 'logical'
  }
  return(imat)
}
