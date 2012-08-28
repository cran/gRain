
.ug2dag <- function(ug){
  m <- mcs(ug)
  if (length(m)==0)
    return(NULL)
  adjList <- adj(ug, m)
  vparList <- vector("list",length(m))
  names(vparList) <- m
  
  ii <- 2
  vparList[[1]] <- m[1]
  for (ii in 2:length(m)){
    vparList[[ii]] <- c(m[ii],intersectPrim(adjList[[ii]], m[1:ii]))
  }
  
  dg <- dagList(vparList)
  dg
}


.eliminationOrder <- function(gg){
  is.acyc <- TRUE
  ### amat <- as.adjmat(gg)
  amat <- as.adjMAT(gg)
  elorder <- NULL

  repeat{
    idx <- which(rowSums(amat)==0)
    if (!length(idx)){
      return(NULL)
    }
    elorder <- c(elorder, idx)
    amat <- amat[-idx,-idx]
  
    if(all(c(0,0)==dim(amat))){
      break()
    }
  }
  names(rev(elorder))
}
