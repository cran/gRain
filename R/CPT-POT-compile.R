compileCPT <- function(x, forceCheck=TRUE, details=0){

  xxx <- lapply(x, .parse.cpt)

  vnamList <- lapply(xxx, "[[", "vnam")
  vparList <- lapply(xxx, "[[", "vpar")
  vn       <- as.vector(unlist(vnamList)) ## ???
  vlevList <- lapply(xxx, "[[", "vlev")
  names(vlevList) <- vnamList
  ans       <- vector("list", length(vnamList))      
  vn        <- c(vnamList, recursive=TRUE)
  di        <- c(lapply(vlevList, length), recursive=TRUE)
  names(di) <- vn

  ## FIXME: Can be parallelized
  if (details>=1) cat(". creating probability tables ...\n")  
  for (ii in 1:length(vnamList)){    
    vpar <- xxx[[ii]]$vpar
    lev  <- vlevList[vpar]
    val  <- xxx[[ii]]$values
    if(details>=2)
      if (ii %% 1000 == 0)
        cat(sprintf(".. ii = %6i, v,pa(v): %s\n", ii, toString(vpar)))

    if (forceCheck){
      mm   <- match(vpar, vn)
      if (any(is.na(mm))){
        sss <- sprintf("compileCPT: Distribution not specified for node(s)\n %s \n",
                       toString(vpar[which(is.na(mm))]))
        stop(sss, call.=FALSE)
      }
      if (prod(c(lapply(lev, length),recursive=TRUE)) != length(val)){
        cat(sprintf("Error for v,pa(v): %s\n", toString(vpar)))
        str(lev);  str(val); stop("Table dimensions do not match!")
      }
    }
    
    ans[[ii]] <- parray(vpar, 
                        values    = val, 
                        normalize = xxx[[ii]]$normalize,
                        smooth    = xxx[[ii]]$smooth, 
                        levels    = lev)
  }

  if (details>=1) cat(". creating dag and checking for acyclicity...\n")

  dg  <- dagList(vparList)

  ## Check for acyclicity
  dgM <- as(dg, "Matrix")
  oo  <- sp_topoSort(dgM)
  if (oo[1]==-1)
    stop("Graph defined by the cpt's is not acyclical...\n");

  universe <- list(nodes = vn, levels = vlevList, nlev   = di)
  attributes(ans) <- list(universe = universe,                          
                          dag      = dg,
                          vparList = vparList)  
  class(ans) <- "CPTspec"
  return(ans)
}

compilePOT <- function(x){

  ## FIXME: compilePOT: Need sparse matrid too
  uug <- ugList(lapply(x, function(a) names(dimnames(a))))
  if (!is.TUG(uug))
    stop("Graph defined by potentals is not triangulated...\n")
  
  lll     <- unlist(lapply(x, dimnames),recursive=FALSE)
  nnn     <- names(lll)
  iii     <- match(unique(nnn), nnn)
  levels  <- lll[iii]
  vn      <- nnn[iii]
  di      <- c(lapply(levels, length), recursive=TRUE)
  names(di) <- vn
  ans       <- x
  universe  <- list(nodes = vn, levels = levels, nlev   = di)
  attributes(ans) <- list(universe=universe,
                          dag    = attr(x, "dag"),                          
                          ug     = uug,
                          ## carry over from extractPOT:
                          cptlist= attr(x, "cptlist"),
                          rip    = attr(x, "rip"))
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

summary.CPTspec <- function(object,...){
  print(object)
  cat(sprintf("attributes:\n %s\n", toString(names(attributes(object)))))
  cat(sprintf("names:\n %s \n", toString(attributes(object)$names)))
  cat(sprintf("nodes:\n %s \n", toString(attributes(object)$nodes)))
  cat(sprintf("levels: \n"))
  str(attributes(object)$levels)
  cat(sprintf("vparList: \n"))
  str(attributes(object)$vparList)
  cat(sprintf("nlev: \n"))
  str(attributes(object)$nlev)
  cat(sprintf("dag: \n"))
  print(attributes(object)$dag)
  cat(sprintf("dagM: \n"))
  print(attributes(object)$dagM)
  return(invisible(object))
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



.parse.cpt <- function(xi){
  if (is.array(xi))
    cls <- "array"
  else
    cls <- class(xi)[1]
  switch(cls,
         "parray"={
           vpar    <- varNames(xi)
           vn      <- vpar[1]
           vparlev <- valueLabels(xi)
           vlev    <- vparlev[[1]]
           values  <- as.numeric(xi)
           tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values,
                           normalize="first", smooth=0)
         },
         "cptable"={
           vpar    <- xi$vpa
           vn      <- vpar[1]
           vlev    <- xi$levels
           tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=xi$values,
                           normalize=if (xi$normalize) "first" else "none", smooth=xi$smooth)
         },
         "array"={
           vpar    <- names(dimnames(xi))
           vn      <- vpar[1]
           vparlev <- dimnames(xi)
           vlev    <- vparlev[[1]]
           values  <- as.numeric(xi)
           tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values,
                           normalize="first", smooth=0)
         })
  return(tmp)
}

