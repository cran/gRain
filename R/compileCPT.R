compileCPT <- function(x){
  ## Does NOT use .cptable (and is hence free of gmData)...
  ## cat("cptspec\n")
  parseit <- function(xi){
    if (is.array(xi))
      cls <- "array"
    else
      cls <- class(xi)[1]
    switch(cls,
           "ptable"={
             vpar <- varNames(xi)
             vn   <- vpar[1]
             vparlev <- valueLabels(xi)
             vlev    <- vparlev[[1]]
             values  <- as.numeric(xi)
             tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values, normalize="first", smooth=0)
           },
           "cptable"={
             vpar <- xi$vpa
             vn   <- vpar[1]
             vlev <- xi$levels
             tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=xi$values,
                             normalize=if (xi$normalize) "first" else "none", smooth=xi$smooth)
           },
           "array"={
             vpar <- names(dimnames(xi))
             vn   <- vpar[1]
             vparlev <- dimnames(xi)
             vlev    <- vparlev[[1]]
             values  <- as.numeric(xi)
             tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values, normalize="first", smooth=0)
           }
           )
    return(tmp)
  }

  xxx <- lapply(x, parseit)

  vnamList <- lapply(xxx, "[[", "vnam")
  vlevList <- lapply(xxx, "[[", "vlev")
  names(vlevList) <- vnamList
  
  ans <- vector("list", length(vnamList))      
  for (ii in 1:length(vnamList)){    
    vpar <- xxx[[ii]]$vpar
    lev  <- vlevList[vpar]
    val  <- xxx[[ii]]$values
    if (prod(c(lapply(lev, length),recursive=TRUE)) != length(val)){
      print(lev)
      print(val)
      stop("Table dimensions do not match!")
    }
    ans[[ii]] <- ptable(vpar, 
                        values    = val, 
                        normalize = xxx[[ii]]$normalize,
                        smooth    = xxx[[ii]]$smooth, 
                        levels    = lev)
  }
  names(ans) <- vnamList
  
                                        #vn.ment  <- uniquePrim(unlist(vparList))    
  vparList <- lapply(xxx, "[[", "vpar")
  valList  <- lapply(xxx, "[[", "values")
  
  dg <- dagList(vparList)
  if (is.null(dg)){
    stop("Graph defined by the cpt's is not acyclical...\n");
  }

  vn <- c(vnamList, recursive=TRUE)
  di <- c(lapply(vlevList, length), recursive=TRUE)
  names(di) <- vn
  
  attributes(ans) <- list(nodes=vn,
                          levels=vlevList,
                          nlev=di,
                          dag=dg)  ## FIXME: nodes can be removed!
  class(ans) <- "cptspec"
  return(ans)
}


compilePOT <- function(x){

  uug <- ugList(lapply(x, function(a) names(dimnames(a))))
  if (length(mcs(uug))==0)
    stop("Graph defined by potentals is not triangulated...\n")
    
  lll <- unlist(lapply(x, dimnames),recursive=FALSE)
  nnn <- names(lll)
  iii <- match(unique(nnn), nnn)
  levels  <- lll[iii]
  vn      <- nnn[iii]
  di <- c(lapply(levels, length), recursive=TRUE)
  names(di) <- vn

  ans <- x
  attributes(ans) <- list(nodes=vn,
                          levels=levels,
                          nlev=di,
                          ug=uug)  ## FIXME: nodes can be removed!
  class(ans) <- "potspec"
  return(ans)
}



print.cptspec <- function(x,...){
  cat("cptspec with probabilities:\n")
  lapply(x,
         function(xx){
           vn <- varNames(xx)
           if (length(vn)>1){
             cat(paste(" P(",vn[1],"|",paste(vn[-1],collapse=' '),")\n"))
           } else {
             cat(paste(" P(",vn,")\n"))
           }
         }
         )
  return(invisible(x))
}


print.potspec <- function(x,...){
  cat("potspec with potentials:\n")
  lapply(x,
         function(xx){
           vn <- names(dimnames(xx))
           cat(   "(", paste(vn, collapse=' '),") \n")
         })
  
  return(invisible(x))
}
