print.ripOrder <- function(x, ...){
  idx <- 1:length(x$cliques)
  cat("Cliques\n")
  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$cliques, idx)
  
  cat("Separators\n")
  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$separators, idx)
  
  cat("Parents\n")
  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$pa, idx)
  
#  cat("Children\n")
#  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$ch, idx)
}





getcpt <- function(bn, v=NULL, pa=NULL){
  if (is.null(v)){
    vpavlist  <- vpav(getSlot(bn,"dag"))
    xx        <- lapply(vpavlist, function(v){ getcpt(bn, v)})
    names(xx) <- sapply(vpavlist, function(d)d[1])
    xx
  } else {
    vvv <- c(v,pa);  #print(vvv)
    qbn <- querygm(bn, vvv, type="joint")
    nst <- nodeStates(bn)[vvv]
    #print(qbn)    #print(nst)
    ctab(vvv, nst, values=qbn$values, normalize="first")
  }
}




eliminationOrder <- function(dag){

  elorder  <- NULL
  is.acyc  <- TRUE
  vpavlist <- vpav(dag)

  repeat{
    v   <-lapply(vpavlist, function(d) d[1])
    pav <- lapply(vpavlist, function(d) d[-1])
    
    vs    <- unlist(v)
    pavs  <- unique(unlist(pav))
    
    sdiff<-setdiff(vs,pavs)
    if (length(sdiff)==0){
      is.acyc <- FALSE
      break()
    }

    elorder <- c(elorder, sdiff[1])    
    idx<-match(sdiff,v)
    vpavlist <- vpavlist[-idx[1]]
    
    if (length(vpavlist)==0)
      break()  
  }
  if (is.acyc)
    return(rev(elorder))
  else
    return(NULL)
}


eliminationOrder <- function(gg){
  is.acyc <- TRUE
  amat <- adjmat(gg)
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




getSlot <- function(x, slot=NULL){
  if (is.null(slot))
    return(x)
  return(x[[slot]])
}



vpav <- function(dag){
  vert <- nodes(dag)
  dagxx  <- c(vert, edges(dag))
  vpalist <- as.list(rep(NA, length(vert)))
  names(vpalist) <- vert
  for (i in 1:length(vert)){
    currv <- vert[i]
    vv<-lapply(dagxx, function(x){
      if (identical(x[1], currv)) x
    })
    vpa <- unlist(vv)
    pa  <- setdiff(vpa, currv)
    vpalist[[i]]<-c(currv, pa)
  }
  return(vpalist)
}





