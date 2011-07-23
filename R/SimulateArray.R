####
#### Re-implementation of simulate() function - quite fast...
#### Bristol, March 2008
####

simulate.grain <- function(object, nsim=1, seed=NULL, ...){

  if (!object$isCompiled){
                                        #cat("Compiling (and propagating) model ...\n")
    object <- compile(object, propagate=TRUE)
  } else {
    if (!object$isPropagated){
                                        # cat("Propagating model...\n")
      object <- propagate(object)
    }
  }

  plist  <- object$equilCQpot
  cqlist <- object$rip$cli
  splist <- object$rip$sep

  ## Init
  ans           <- matrix(0, nr=nsim, nc=length(nodeNames(object)))
  colnames(ans) <- nodeNames(object)

  ctab  <- plist[[1]]
  res   <- .simarray(x=ctab,n=nsim)
  ans[,colnames(res)] <- res
  
  ## Iterate
  if (length(cqlist)>1){
    for (ii in 2:length(cqlist)){
      ctab <- plist[[ii]]
      vn   <- names(dimnames(ctab)) # Safe
      s2   <- splist[[ii]]
      mtab <- tableMargin(ctab,s2)      ## FIXME: Check this
      ctab <- tableOp2(ctab, mtab, `/`) ## FIXME: Check this
      
      r2   <- setdiff(vn, s2)
      ##cat("r:", r2, "s:", s2, "\n")    
      if (length(s2)){
        s2idx <- match(s2, vn)
        res   <- matrix(0,nr=nsim, nc=length(r2))
        colnames(res) <- r2
        un    <- ans[,s2,drop=FALSE]
        vals  <- unique(un)
        sc    <- cumprod(apply(vals, 2, max) )
        key   <- un %*% sc / sc[1]

        for(k in unique(key)) 
          res[k==key,] <- .simarray(ctab, sum(k==key), s2idx, un[match(k,key),])
      } else {
        res <- .simarray(x=ctab,n=nsim)
      }
      ans[,colnames(res)] <- res
    }
  }

  ns <- nodeStates(object)

  ans <- as.data.frame(ans)
  vn <- names(ans)
  
  for (jj in 1:ncol(ans)){
    match(vn[jj], names(ns))
    ans[,jj] <- factor(ans[,jj], levels=seq(ns[[jj]]))
    levels(ans[,jj]) <- ns[[jj]]
  }

  return(ans)
}
