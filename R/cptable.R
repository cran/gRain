cptable <- function(v, pa=NULL, levels=NULL, values=NULL,
                normalize=TRUE,
                smooth=0                
                ){

  vpa <- c(.formula2character(v),.formula2character(pa))
  ans <- list(vpa=vpa, values=values, normalize=normalize, smooth=smooth, levels=levels)
  class(ans) <- "cptable"
  return(ans)

}

print.cptable <- function(x,...){
  cat("vpa    :", x$vpa ,"\n")
  cat("values :", x$values, "\n")
  cat(sprintf("levels (%s) : %s\n", x$vpa[1], paste(x$levels,collapse=' ')))
  cat("normalize :", x$normalize, "smooth :", x$smooth,"\n")
  return(invisible(x))
}



.cptable <- function(v, pa=NULL, values=NULL,
                gmData=NULL, 
                normalize=TRUE,
                smooth=0, 
                levels=NULL
                ){

  vpa <- c(.formula2character(v),.formula2character(pa))

  
  nvpa <- length(vpa)

  if (!is.null(gmData)){
    uuu <- match(vpa, varNames(gmData))
    if (any(is.na(uuu)))
      stop("Nodes {",paste(vpa[is.na(uuu)],collapse=','), "} do not exist in gmData\n")
    levels <- valueLabels(gmData)[vpa]
    ans    <- ptable(vpa, levels, values, smooth=smooth,
                     normalize=if (normalize) "first" else "none")  
  } else {

    if (!inherits(levels,"list")){
      dn        <- vector("list",nvpa)
      names(dn) <- vpa 
      for (ii in 1:nvpa){
        dn[[ii]] <- levels
      }
    } else {
      dn <- levels
    }
    dims <- unlistPrim(lapply(dn,length))
    
    vl <- prod(dims)
    if (length(values) >1 && length(values) > vl) {
      stop(sprintf("length of 'values' (%i) exceeds array dimension (%i) \n", length(values), vl))
    }

    values <- rep(values, length.out = vl)
    ans <- ptable(vpa, levels=dn, values=values, smooth=smooth,
                  normalize=if (normalize) "first" else "none")

  }
  return(ans)
}

## .cptspec <- function(x){
##   vn <- sapply(lapply(x, varNames),    "[[", 1)
##   vl <- lapply(lapply(x, valueLabels), "[[", 1)

##   dg <- dagList(lapply(x, varNames))
##   if (is.null(dg)){
##     stop("Graph defined by the cpt's is not acyclical...\n");
##   }

##   ## FIXME: There could be some consistency checking here...
##   attributes(x) <- list(nodes=vn, levels=vl, dag=dg)
##   names(x) <- vn
##   class(x) <- "cptspec"
##   x
## }


