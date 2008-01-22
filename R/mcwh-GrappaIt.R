ripOrderGreen <- function(ug, vn=nodes(ug), nLevels=rep(2,length(vn)),control=list()){

  t0 <- proc.time()
  amat  <- adjmat(ug, vn)
  if (!is.null(control$timing) && control$timing)
    cat(" Time: (detail) Adjancency matrix:", proc.time()-t0,"\n") 

  t0 <- proc.time()
  ans   <- mcwhSH(amat, nLevels, quiet=TRUE)
  if (!is.null(control$timing) && control$timing)
    cat(" Time: (detail) mcwh:", proc.time()-t0,"\n")

  
  ##t0 <- proc.time()
  cq    <- lapply(ans$j.cq, function(a) vn[a])
  sp    <- lapply(ans$j.sp, function(a) vn[a])
  sp[1] <- NA
  ##jtree <- ans$j.tree

  #x   <- do.call("rbind", lapply(jtree,function(xx)xx[1,]))
  x   <- do.call("rbind", lapply(ans$j.tree,function(xx)xx[1,]))

  pa  <- c(NA,x[-1,2])
  ch  <- x[,1]

  rip2 <- structure(list(nodes=vn,               
               cliques=cq,
               separators=sp,
               pa=pa,
               ch=ch), class="ripOrder")

  return(rip2)
}



