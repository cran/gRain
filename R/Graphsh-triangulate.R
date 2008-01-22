
triangulate <- function(ug, root=NULL){
  if (!is.null(root)){
    if (length(root)>1)
      ug <- updateugsh(ug, root)
  }
  tris(ug)
}

## Peters triangulation
##
trip <- function(ug){
  jt <- junctionTree(ug)
  cq <- jt$cliques
  tug <- newugsh(cq)
  return(tug)
}

## Sørens triangulation
##
tris <- function(ug, vn=nodes(ug), nLevels=rep(2, length(vn))){
  
  amat    <- amat2 <- adjmat(ug)
  anodes  <- vn 
  names(nLevels) <- vn

  activeList <- rep(1, length(vn))
  gnodes     <- rep(1, length(vn))
  names(activeList) <- names(gnodes) <- vn
  wgt   <- rep(NA,length(vn))
  names(wgt) <- vn
  repeat{
    #cat("RUN\n")
    #cat("activeList:", paste(vn[activeList==1], collapse=' '),"\n")
    #print(activeList)
    #print(gnodes)
    for (ii in 1:length(anodes)){
      cn <- anodes[ii]
      if (activeList[cn]==1){
        #nb <-  intersect(anodes, names(which(amat[cn,]==1)))
        #print(as.numeric(amat[cn,]))
        #print(as.numeric(amat[cn,])* gnodes)
        #print((as.numeric(amat[cn,])* gnodes)==1)
        nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
        #nb <-  names((as.numeric(amat[cn,]) * gnodes)==1)
        w  <-  prod(nLevels[c(cn,nb)])
        wgt[cn] <- w 
     #   cat("cn:", cn, "nb:", paste(nb, collapse=' '), "wgt:", w, "\n")
        activeList[cn] <- 0
      }
    }
#    print(wgt)
    id    <- which.min(wgt)
    wgt[id] <- Inf

#    print(id)
    cn <- vn[id]
    nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
   # nb <- intersect(anodes, names(which(amat[cn,]==1)))
    activeList[cn] <- -1
    activeList[nb] <-  1
    
 #   cat("completing bd for node:", cn, "nb:", paste(nb, collapse=' '), "\n")
    
    if (length(nb)>1){
      for (i in 1:(length(nb)-1)){
        for (j in (i+1):length(nb)){
          amat2[nb[i],nb[j]] <- amat2[nb[j],nb[i]] <- TRUE
          amat [nb[i],nb[j]] <- amat [nb[j],nb[i]] <- TRUE
        }
      }
    }

    gnodes[id] <- 0
    #print(anodes)
    anodes <- setdiff(anodes,cn)
    if (length(anodes)==1) 
      break()
#    amat   <- amat[anodes, anodes]
  }

  
  amat2[lower.tri(amat2)] <- FALSE
  edmat <- edmat2 <- which(amat2, arr.ind=TRUE)
  storage.mode(edmat2) <- "character"
  dimnames(edmat2) <- NULL
  edmat2[,1] <- vn[edmat[,1]]
  edmat2[,2] <- vn[edmat[,2]]
  ed <- split(edmat2, row(edmat2))
  tug <- new("ugsh", nodes=vn, edges=ed)
  ##print("DONE")
  return(tug)
}



