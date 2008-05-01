
triangulate <- function(ug, method="standard",
                        nLevels=rep(2,length(nodes(ug))), matrix=FALSE){

  trimethod <- c("standard","mcwh","r")
  method <- match.arg(tolower(method),trimethod)


  switch(method,
         "mcwh"={ ## Peter Greens code
           ans <- triangPG(ug,nLevels=nLevels)
           if (matrix)
             ans <- as.adjmat(ans)
         },
         "r"={ ## Pure R implementations
           ans <- triangR(ug,nLevels=nLevels)
           if (matrix)
             ans <- as.adjmat(ans)
         },
         "standard"={

           A  <- as.adjmat(ug)

           Av <- as.numeric(A)

           nc <- ncol(A)
           vn <- colnames(A)

           ##print(nLevels)
           i  <-.C("triangmcwh", Av=as.integer(Av), nc, vn,
                   as.integer(nLevels), ans=integer(1), PACKAGE="gRain")$Av
           ans  <-matrix(i, nc=nc,nr=nc)
           dimnames(ans)<-dimnames(A)
           if (!matrix)
             ans <- as.grash(ans)           
         }
         )
  return(ans)
}


## Triangulation based on Peter Greens code
##

triangPG <- function(ug,nLevels=rep(2,length(nodes(ug)))){
  jt  <- jTree(ug, method="mcwh", nLevels=nLevels)
  cq  <- jt$cliques
  tug <- newugsh(cq)
  return(tug)
}

## Sørens triangulation
##
triangR <- function(ug, vn=nodes(ug), nLevels=rep(2, length(vn))){
  
  amat    <- amat2 <- as.adjmat(ug)
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




jTree <- function(ug,
                         method  = "standard",
                         vn      = nodes(ug),
                         nLevels = rep(2,length(vn)),
                         control = list()){
  trimethod <- c("standard","mcwh","r")
  method <- match.arg(tolower(method),trimethod)

  switch(method,
         "standard"=,
         "r"={
           tug        <- triangulate(ug, method=method,nLevels=nLevels)

           val        <- ripOrder(tug,nLevels=nLevels)
           val$tug    <- tug
           return(val)
         },
         "mcwh"={
           val         <- ripOrderGreen(ug,vn,nLevels,control)
           val$nLevels <- nLevels
           val$tug     <- newugsh(val$cliques)
           return(val)           
         }
         
         )
}




