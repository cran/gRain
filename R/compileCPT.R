compileCPT <- function(x){
  parseit <- function(xi){
    if (is.array(xi))
      cls <- "array"
    else
      cls <- class(xi)[1]
    switch(cls,
           "parray"={
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
             tmp  <- list(vnam=vn, vlev=vlev, vpar=vpar, values=xi$values,
                          normalize=if (xi$normalize) "first" else "none", smooth=xi$smooth)
           },
           "array"={
             vpar    <- names(dimnames(xi))
             vn      <- vpar[1]
             vparlev <- dimnames(xi)
             vlev    <- vparlev[[1]]
             values  <- as.numeric(xi)
             tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values, normalize="first", smooth=0)
           })
    return(tmp)
  }
  
  xxx <- lapply(x, parseit)
  
  vnamList <- lapply(xxx, "[[", "vnam")
  vlevList <- lapply(xxx, "[[", "vlev")
  names(vlevList) <- vnamList
  
  ans <- vector("list", length(vnamList))      

  vn <- as.vector(unlist(vnamList))
##   cat("Nodes:\n")
##   print(vn)
  
  for (ii in 1:length(vnamList)){    
    vpar <- xxx[[ii]]$vpar
    lev  <- vlevList[vpar]
    val  <- xxx[[ii]]$values
    #cat(sprintf("v,pa(v): %s\n", toString(vpar)))
    #str(lev)

    mm <- match(vpar, vn)
    isna <- is.na(mm)
    if (any(isna)){
      sss <- sprintf("compileCPT: Distribution not specified for node(s)\n %s \n", toString(vpar[which(isna)]))
      stop(sss, call.=FALSE)
    }
    
    if (prod(c(lapply(lev, length),recursive=TRUE)) != length(val)){
      cat(sprintf("Error for v,pa(v): %s\n", toString(vpar)))
      str(lev)
      str(val)
      stop("Table dimensions do not match!")
    }

    ans[[ii]] <-
      parray(vpar, 
             values    = val, 
             normalize = xxx[[ii]]$normalize,
             smooth    = xxx[[ii]]$smooth, 
             levels    = lev)
  }
  
  vparList <- lapply(xxx, "[[", "vpar")
  valList  <- lapply(xxx, "[[", "values")
  
  dg <- dagList(vparList)
  if (is.null(dg)){
    stop("Graph defined by the cpt's is not acyclical...\n");
  }
  
  vn <- c(vnamList, recursive=TRUE)
  di <- c(lapply(vlevList, length), recursive=TRUE)
  names(di) <- vn
  
  attributes(ans) <- list(names=vn,
                          nodes=vn,
                          levels=vlevList,
                          nlev=di,
                          dag=dg)  ## FIXME: nodes can be removed!
  class(ans) <- "CPTspec"
  return(ans)
}

compilePOT <- function(x){

  uug <- ugList(lapply(x, function(a) names(dimnames(a))))
  if (length(mcs(uug))==0)
    stop("Graph defined by potentals is not triangulated...\n")
  
  lll     <- unlist(lapply(x, dimnames),recursive=FALSE)
  nnn     <- names(lll)
  iii     <- match(unique(nnn), nnn)
  levels  <- lll[iii]
  vn      <- nnn[iii]
  di      <- c(lapply(levels, length), recursive=TRUE)
  names(di) <- vn
  ans       <- x
  attributes(ans) <- list(nodes  = vn,
                          levels = levels,
                          nlev   = di,
                          ug     = uug,
                          dag    = attr(x, "dag"),
                          cptlist= attr(x, "cptlist"),
                          rip    = attr(x, "rip")
                          )
  ## FIXME: nodes can be removed!
  ## FIXME: We carry dag+cptlist here (created in extractPOT); maybe not so elegant.
  class(ans) <- "POTspec"
  return(ans)
}


print.CPTspec <- function(x,...){
  cat("CPTspec with probabilities:\n")
  lapply(x,
         function(xx){
           vn <- varNames(xx)
           if (length(vn)>1){
             cat(paste(" P(",vn[1],"|",paste(vn[-1],collapse=' '),")\n"))
           } else {
             cat(paste(" P(",vn,")\n"))
           }
         })
  return(invisible(x))
}

print.POTspec <- function(x,...){
  cat("POTspec with potentials:\n")
  lapply(x,
         function(xx){
           vn <- names(dimnames(xx))
           cat(   "(", paste(vn, collapse=' '),") \n")
         })
  
  return(invisible(x))
}
