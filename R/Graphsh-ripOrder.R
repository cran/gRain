
## Works only on triangulated graph
##
## Based on Algorithm 4.11 in Steffen et all (the yellow book)
##

ripOrder <- function(ug, root=NULL,nLevels=NULL){

  mc <- mcs(ug,root)

  amat <- adjmat(ug)
  amat <- amat[mc,mc]
  lenmc  <- length(mc)
  
  bd <- ladder <- rep(0,lenmc)
  ladder[lenmc] <- 1
  
  
  for (i in 1:lenmc){
    bd[i] <- sum(amat[i, 1:(i-1)])
  }
  
  for (i in 1:(lenmc-1)){
    ladder[i] <- (bd[i] + 1 > bd[i+1])
  }
  
  idx <- which(ladder>0)
  lenidx <- length(idx)
  
  pa <- rep(NA, lenidx)
  cq <- sp <- as.list(pa)
  
  for(i in 1:lenidx){
    ii <- idx[i]
    cq[[i]] <- c(c(which(amat[ii,1:(ii-1)])),ii)
  }
  
  cq <- lapply(cq, as.numeric)
  
  cqmat <- cliquemat(cq, mc)

  #print(cqmat); print(".........")
  #cqmat <<- cqmat
  #cq <<- cq
  #mc <<- mc
  if (length(cq)>1){
    for (i in 2:length(cq)){
      ccq <- mc[which(cqmat[i,]==1)]
      pamat <- cqmat[1:(i-1),,drop=FALSE]
  #print(pamat)
      pai <- mc[c(which(colSums(pamat)==1),i)]
      isect <- intersect(ccq,pai)
      ##print(isect)
      if (length(isect)){
        pa[i] <- which.max(rowSums(pamat[,isect,drop=FALSE]))
        ##which.max(rowSums(pamat[,isect,drop=FALSE])>0)[1]
      }
    }
  }
  
  for (i in 1:length(cq)){
    if (!is.na(pa[i])){
      sp[[i]] <- intersect(cq[[i]], cq[[pa[i]]])
    }
  }
  
  cq    <- lapply(cq, function(a) mc[a])
  sp    <- lapply(sp, function(a) if(length(a)==1 && is.na(a)) NA else mc[a])
  
  rip2 <- structure(list(nodes=mc,               
                         cliques=cq,
                         separators=sp,
                         pa=pa,
                         nLevels=nLevels,
                         ch=0), class="ripOrder")
  return(rip2)
}


