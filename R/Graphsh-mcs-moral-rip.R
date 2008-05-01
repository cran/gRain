

moralize <- function(dag){

  amat2 <- amat  <- as.adjmat(dag)
  for(k in 1:ncol(amat)){
    idx <- which(amat[,k]==1)
    lenidx <- length(idx)
    if (lenidx>1){ 
      for (i in 1:(lenidx-1)){
        for (j in (i+1):lenidx) {
          amat2[idx[i],idx[j]] <- TRUE
        }
      }
    }
  }
  
  vn      <- colnames(amat2)
  amat2   <- amat2 + amat + t(amat2 + amat)
  amat2[lower.tri(amat2)]<-0
  arrind <-  which(amat2>0, arr.ind=TRUE)
  medges <- cbind(vn[arrind[,1]],vn[arrind[,2]])
  medges <- split(medges, row(medges))
  
  value <- new("ugsh", gens=NULL, nodes=nodes(dag), edges=medges)
  value
}











##
## Maximum Cardinality Search, October, 2007
##
## Returns perfect ordering if it exists and NULL otherwise
##





mcs <- function(ug, amat=as.adjmat(ug), vn=colnames(amat), root=NULL, index=FALSE){
  
  is.perfect <- TRUE

  if (!is.null(root))
    cnode <- match(root, vn)
  else
    cnode <- 1

  active  <- nnvec <- rep(0,length(vn))
  passive <- rep(1,length(vn))
  ans     <- NULL

  amat <- amat*1
                                        #print(amat)
  
  for (kk in 1:length(vn)){
                                        #cat("cnode:", cnode, "\n")
    ans <- c(ans, cnode)
    active[cnode] <- 1
    passive[cnode] <- 0 
    nb  <- amat[cnode,]
    
    is.comp <- TRUE
    nbidx <- which((nb*active)==1)
                                        #print(nbidx)
    len   <- length(nbidx)
    if (len>1){
      for (ii in 1:(len-1)) {
                                        #cat ("ii", ii, "vnii:", vn[nbidx[ii]], "\n")
        for(jj in (ii+1):(len)) {
                                        #cat ("  jj", jj, "vnjj:", vn[nbidx[jj]], "\n")
          if (amat[nbidx[ii],nbidx[jj]]==0){
            is.comp <- FALSE
            break()
          }
        }
      }
    }
    is.perfect <- is.comp
    
    if (!is.perfect){
      #cat("NOT perfect\n")
      #print(cnode)

      break()
    }
    nnvec <- nnvec + nb
    if (max(nnvec * passive)==0){
      cnode <- which(passive==1)[1]
    } else {
      cnode <- which.max(nnvec * passive)
    }
    vn[ans]
  }
  
  if (is.perfect){
    if (index)
      return(ans)
    else
      return(vn[ans])
  } else {
    return(NULL)
  }
}








##
## Works only on triangulated graph
##
## Based on Algorithm 4.11 in Steffen et all (the yellow book)
##

ripOrder <- function(ug, root=NULL,nLevels=NULL){

  #subsetof3 <- function(g1, g2){
  #  all(.Internal(match( g1, g2, 0))>0)
  #}
  
  t0 <- proc.time()
  amat <- as.adjmat(ug)
                                        #cat("finding amat", proc.time()-t0,"\n"); t0 <- proc.time()
  mcidx <- mcs(ug,amat=amat, root=root, index=TRUE)
                                        #cat("finding mcs", proc.time()-t0,"\n"); t0 <- proc.time()

  #cat("mcs", proc.time()-t0,"\n"); t0 <- proc.time()
  if (is.null(mcidx))
    return(NULL)
  vn <- nodes(ug)
  

  len <- length(mcidx)
  ladder <- is.ladder <- rep.int(0, len)
  is.ladder[len] <- 1
  
  cq <- list()
  cqcount <- 1
  for (ii in len:1){
    nb   <- amat[mcidx[ii],]
    prev <- rep(0, len)
    if (ii > 1){
      prev[mcidx[1:(ii-1)]] <- 1
      prevnb <- nb*prev
      ladder[ii] <- sum(prevnb)
    }
    if (ii == len){
      cq[[cqcount]] <- c(mcidx[ii],which(prevnb==1))
      cqcount <- cqcount + 1
    } else {
      xx <- (ladder[ii] + 1 > ladder[ii+1])    #print(xx)
      if (xx){ #print (mcidx[ii]); print (which(prevnb==1))
        cq[[cqcount]] <- c(mcidx[ii],which(prevnb==1))
        cqcount <- cqcount + 1
      }
      is.ladder[ii] <- xx
    }
  }
  
  cq <- rev(cq)
  cq <- lapply(cq, function(x) {names(x)<-NULL; x})

  #cat("finding cliques", proc.time()-t0,"\n"); t0 <- proc.time()
  
  ncq <- length(cq)
  sp  <- as.list(rep(NA, ncq))
  pa  <- rep(NA, ncq)
  for (ii in 2:ncq){
    paset <- unlist(cq[1:(ii-1)])
    isect <- intersect(cq[[ii]], paset)
    sp[[ii]] <- isect  
    if (length(isect)){
      for (kk in (ii-1):1){  #print("----");print(kk); print(cq[[kk]]); print(isect)
        if (subsetof(isect,cq[[kk]])){
          pa[ii]   <- kk  
          break()    
        }
      }
    }
  }
  #cat("finding sep/pa", proc.time()-t0,"\n"); t0 <- proc.time()
  
  sp[sapply(sp, length)==0] <- NA

  cq    <- lapply(cq, function(a) vn[a])
  sp    <- lapply(sp, function(a) if(length(a)==1 && is.na(a)) NA else vn[a])

  
  rip2 <-
    structure(list(nodes      =vn[mcidx],               
                   cliques    =cq,
                   separators =sp,
                   pa         =pa,
                   nLevels    =nLevels
                   ),
              class="ripOrder")
  
  return(rip2)

}


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



# ##
# ## Below: Old version of ripOrder. The new one appears about 3 times faster.
# ##
# .ripOrder <- function(ug, root=NULL,nLevels=NULL){

#   t0 <- proc.time()
#   amat <- as.adjmat(ug)
#   mc <- mcs(ug,amat=amat, root=root)

#   if (is.null(mc))
#     return(NULL)
  
#   amat   <- amat[mc,mc]
#   lenmc  <- length(mc)
  
#   bd <- ladder <- rep(0,lenmc)
#   ladder[lenmc] <- 1
  
#   for (i in 1:lenmc){
#     bd[i] <- sum(amat[i, 1:(i-1)])
#   }
  
#   for (i in 1:(lenmc-1)){
#     ladder[i] <- (bd[i] + 1 > bd[i+1])
#   }
  
#   idx <- which(ladder>0)
#   lenidx <- length(idx)
  
#   pa <- rep(NA, lenidx)
#   cq <- sp <- as.list(pa)
  
#   for(i in 1:lenidx){
#     ii <- idx[i]
#     cq[[i]] <- c(c(which(amat[ii,1:(ii-1)])),ii)
#   }
#   cq <- lapply(cq, as.numeric)

#   ## cqmat is a matrix representing the cliques of the graph
#   ##

# #  print(cq)
# #  print(mc)
#   cqmat <- cliquemat(cq, mc)
# #  print(cqmat)
# #  cat("finding cliques", proc.time()-t0,"\n"); t0 <- proc.time()
  
  
#   if (length(cq)>1){
#     for (i in 2:length(cq)){
#       ccq <- mc[which(cqmat[i,]==1)]
#       pamat <- cqmat[1:(i-1),,drop=FALSE]
#                                         #print(pamat)
#       pai <- mc[c(which(colSums(pamat)==1),i)]
#       isect <- intersect(ccq,pai)
#                                         #print(isect)
#       if (length(isect)){
#         pa[i] <- which.max(rowSums(pamat[,isect,drop=FALSE]))
#                                         #which.max(rowSums(pamat[,isect,drop=FALSE])>0)[1]
#       }
#     }
#   }
  
#   for (i in 1:length(cq)){
#     if (!is.na(pa[i])){
#       sp[[i]] <- intersect(cq[[i]], cq[[pa[i]]])
#     }
#   }
  
#   cq    <- lapply(cq, function(a) mc[a])
#   sp    <- lapply(sp, function(a) if(length(a)==1 && is.na(a)) NA else mc[a])

# #  cat("finding rip", proc.time()-t0,"\n"); t0 <- proc.time()
  
#   rip2 <-
#     structure(list(nodes      =mc,               
#                    cliques    =cq,
#                    separators =sp,
#                    pa         =pa,
#                    nLevels    =nLevels,
#                    ch         =0),
#               class="ripOrder")
#   return(rip2)
# }










######################################################################
######################################################################
######################################################################
######################################################################
######################################################################


