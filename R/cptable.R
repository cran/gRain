cptable <- function(v, levels=NULL, values=NULL,
                normalize=TRUE,
                smooth=0                
                ){

#  vpa  <- c(.formula2char(v),.formula2char(pa))
  vpa  <- c(.formula2char(v))
  ans  <- list(vpa=vpa, values=values, normalize=normalize, smooth=smooth, levels=levels)
  class(ans) <- "cptable"
  return(ans)

}

print.cptable <- function(x,...){
  cat(sprintf("v,pa(v)        : %s\n", toString(x$vpa)))
  cat(sprintf("levels of v    : %s\n", toString(x$levels)))
  cat(sprintf("values         : %s\n", toString(x$values)))
  cat(sprintf("normalize=%s, smooth=%f\n", x$normalize, x$smooth))
  return(invisible(x))
}



.cptable <- function(v, values=NULL,
                gmData=NULL, 
                normalize=TRUE,
                smooth=0, 
                levels=NULL
                ){

#  vpa <- c(.formula2char(v),.formula2char(pa))
  vpa <- c(.formula2char(v))

  
  nvpa <- length(vpa)

  if (!is.null(gmData)){
    uuu <- match(vpa, varNames(gmData))
    if (any(is.na(uuu)))
      stop("Nodes {",paste(vpa[is.na(uuu)],collapse=','), "} do not exist in gmData\n")
    levels <- valueLabels(gmData)[vpa]
    ans    <- parray(vpa, levels, values, smooth=smooth,
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
    ans <- parray(vpa, levels=dn, values=values, smooth=smooth,
                  normalize=if (normalize) "first" else "none")

  }
  return(ans)
}


