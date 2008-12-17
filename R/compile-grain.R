##
## Compiling grain
##

compile.grain <- function(object, 
                          method="mcwh", propagate=FALSE, root=NULL, smooth=0,
                          control=object$control, trace=0,...)
{
  method <- match.arg(tolower(method), c("mcwh","r"))

  if (inherits(object, "cpt-grain")){
    .compileCPT(object, method=method, propagate=propagate, root=root, smooth=smooth, control=control, trace=trace)
  } else {
    if (inherits(object, "dag-grain")){
      object$cptlist   <- dag2cptspec(object$dag,object$gmData,smooth=smooth)
      .compileCPT(object, method=method, propagate=propagate, root=root, smooth=smooth, control=control, trace=trace)      
    } else {
      ## UG
      .compileUG(object, method=method, propagate=propagate, root=root, smooth=smooth, control=control, trace=trace)      
    }
  }  
}
                          
.compileCPT <- 
  function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=x$control, trace=0)
{
  t00 <-  proc.time()

  ## FIXME: Do we need elimination order???
  ##
  t0 <- proc.time()
  elorder    <- eliminationOrder(x$dag)  
  if (!is.null(control$timing) && control$timing)
    cat("Time: elimination order:", proc.time()-t0,"\n")

  ## Moralize DAG
  ##
  t0 <- proc.time()
  mdag  <- moralize(x$dag)
  if (!is.null(control$timing) && control$timing)
    cat("Time: moralize:", proc.time()-t0,"\n")
    
  ## Add edges between variables in 'root' to the moralized graph    
  ##
  if (!is.null(root) && length(root)>1){
    rootEdges <- combn(root,2)
    for (jj in 1:ncol(rootEdges)){
      if (!isAdjacent(mdag, rootEdges[1,jj],rootEdges[2,jj]))
        mdag <- addEdge(rootEdges[1,jj],rootEdges[2,jj], mdag)
    }
  }  

  ## Triangulate graph and create rooted junction tree
  ##
  t0 <- proc.time()
  vn   <- nodes(mdag)
  nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]
  ug   <- triangulate(mdag, method=method, nLevels=nLev)
  rip  <- RIP(ug, nLevels = nLevels)
                                        #rip <- jTree(mdag, method=method, nLevels=nLev,  control=control)
  if (!is.null(control$timing) && control$timing)
    cat("Time: triangulate:", proc.time()-t0,"\n")
  
  ## Create initial list of potentials (all with 1's in all cells)
  ##
  t0 <- proc.time()
  dummypotlist <- .createPotentialList(rip,x$gmData)
  if (!is.null(control$timing) && control$timing)
    cat("Time: Create potentials:", proc.time()-t0,"\n")

  ## Insert CPT's in potential list
  t0 <- proc.time()
  potlist     <-  potlistwork <- potlistorig <- .insertCpt(x$cptlist, dummypotlist, rip, trace)
  if (!is.null(control$timing) && control$timing)
    cat("Time: Insert cpt into potentials:", proc.time()-t0,"\n") 

  ## Collect results
  ##
  ans      <- list(
                   rip         = rip,
                   ug          = ug,
                   potlist     = potlist,
                   potlistwork = potlistwork,
                   potlistorig = potlistorig, 
                   mdag        = mdag,
                   elorder     = elorder,
                   trace       = trace,
                   initialized = FALSE,
                   propagated  = FALSE
                   )
  
  ans        <- c(x, ans)
  class(ans) <- c('compgrain', class(x))

  ## Propagate if asked to
  ##
  if (propagate){
    if (trace>=1) cat (".Initializing network\n")
    ans             <- propagate(ans)
  }

  ## Total time
  ##
  if (!is.null(control$timing) && control$timing)
    cat("Time: (total) compile:", proc.time()-t00,"\n")

  ## Done
  ##
  return(ans)
}


.compileUG <-
  function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=x$control, trace=0)
{

  t00 <-  proc.time()
  
  ## Check if graph is triangulated
  ##
  if (length(MCSMAT(as.adjMAT(x$ug)))==0){
    cat("Undirected graph is not triangulated...\n"); return(NULL)
  }

  ## Just info
  ##
  if (!is.null(root))
    cat("Specifying 'root' has no effect on models defined from an undirected graph\n")

  ## Triangulate graph and create rooted junction tree
  ##
  t0   <- proc.time()
  vn   <- nodes(x$ug)
  nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]
  rip  <- RIP(x$ug, nLevels = nLevels)

  #rip  <- jTree(x$ug, method=method, nLevels=nLev,  control=control)
  if (!is.null(control$timing) && control$timing)
    cat("Time: triangulate:", proc.time()-t0,"\n")

  ## Extract clique potentials from data
  ##
  t0 <- proc.time()
  potlist <- potlistwork <- potlistorig <- ug2potspec(x$ug, x$gmData, rip, smooth=smooth)
  if (!is.null(control$timing) && control$timing)
    cat("Time: create potentials:", proc.time()-t0,"\n")

  ## Collect results
  ##  
  ans <- list(rip         = rip,
              ug          = x$ug,
              potlist     = potlist,
              potlistwork = potlistwork,
              potlistorig = potlistorig,
              trace       = trace,
              initialized = FALSE,
              propagated  = FALSE
              )
  ans <- c(x, ans)
  class(ans) <- c('compgrain', class(x))

  ## Propagate if asked to
  ##  
  if (propagate){
    if (trace>=1) cat (".Initializing network\n")
    ans             <- propagate(ans)
  }

  ## Total time
  ##
  if (!is.null(control$timing) && control$timing)
    cat("Time: (total) compile:", proc.time()-t00,"\n")

  ## Done
  ##  
  return(ans)
}












## compilegm <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##             control=x$control, trace=0){
##     UseMethod("compilegm")
## }

## "compilegm.dag-grain" <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##            control=x$control, trace=0){

##     cptlist   <- dag2cptspec(x$dag,x$gmData,smooth=smooth)
##     x$cptlist <- cptlist
    
##     "compilegm.cpt-grain"(x, method=method, propagate=propagate,
##                           root=root, smooth=smooth, trace=trace)
##   }


## "compilegm.cpt-grain" <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##            control=x$control, trace=0)
## {
  
##     ##cat("compilegm.cpt-grain\n")
##     t00 <- t0 <- proc.time()
##     method <- match.arg(tolower(method), c("mcwh","r"))
##     elorder    <- eliminationOrder(x$dag)
    
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: elimination order:", proc.time()-t0,"\n")
    
##     t0 <- proc.time()
##     mdag       <- moralize(x$dag)
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: moralize:", proc.time()-t0,"\n")
    
##     ## Add edges between variables in 'root' to the moralized graph    
##     ##
##     if (!is.null(root) && length(root)>1){
##       rootEdges <- combn(root,2)
##       for (jj in 1:ncol(rootEdges)){
##         if (!isAdjacent(mdag, rootEdges[1,jj],rootEdges[2,jj]))
##           mdag <- addEdge(rootEdges[1,jj],rootEdges[2,jj], mdag)
##       }
##     }  

##     vn   <- nodes(mdag)
##     nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]
##     rip <- jTree(mdag, method=method, nLevels=nLev,  control=control)

##     if (!is.null(control$timing) && control$timing)
##       cat("Time: triangulate:", proc.time()-t0,"\n")

##     t0 <- proc.time()
##     dummypotlist <- .createPotentialList(rip,x$gmData)
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: Create potentials:", proc.time()-t0,"\n")

##     t0 <- proc.time()
##     potlist     <-  potlistwork <- potlistorig <- .insertCpt(x$cptlist, dummypotlist, rip, trace)
    
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: Insert cpt into potentials:", proc.time()-t0,"\n") 

##     ans      <- list(rip         = rip,
##                      potlist     = potlist,
##                      potlistwork = potlistwork,
##                      potlistorig = potlistorig, 
##                      mdag        = mdag,
##                      elorder     = elorder,
##                      trace       = trace,
##                      initialized = FALSE,
##                      propagated  = FALSE)
    
##     ans        <- c(x, ans)
##     class(ans) <- c('compgrain', class(x))

##     if (propagate){
##       if (trace>=1) cat (".Initializing network\n")
##       ans             <- propagate(ans)
##     }
    
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: (total) compile:", proc.time()-t00,"\n")

##     return(ans)
##   }


## "compilegm.ug-grain" <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##            control=x$control, trace=0)
## {
##   ##cat("compilegm.ug-grain\n")
  
##   method <- match.arg(tolower(method), c("mcwh","r"))
  
##   if (length(MCSMAT(as.adjMAT(x$ug)))==0){
##     cat("Undirected graph is not triangulated...\n"); return(NULL)
##   }
  
##   if (!is.null(root)){
##     cat("Specifying 'root' has no effect on models defined from an undirected graph\n")
##   }

##   t0   <- proc.time()
##   vn   <- nodes(x$ug)
##   nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]
##   rip  <- jTree(x$ug, method=method, nLevels=nLev,  control=control)
  
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: triangulate:", proc.time()-t0,"\n")
  
##   t0 <- proc.time()
##   potlist <- potlistwork <- potlistorig <- ug2potspec(x$ug, x$gmData, rip, smooth=smooth)
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: create potentials:", proc.time()-t0,"\n")
  
##   ans <- list(rip         = rip,
##               potlist     = potlist,
##               potlistwork = potlistwork,
##               potlistorig = potlistorig,
##               initialized = FALSE,
##               propagated  = FALSE
##               )
  
##   ans <- c(x, ans)
##   class(ans) <- c('compgrain', class(x))
  
##   if (propagate){
##     if (trace>=1) cat (".Initializing network\n")
##     ans             <- propagate(ans)
##   }
##   return(ans)
## }






## UPS - dette virker ikke.... Hvorfor...
##x$cptlist  <- cptspec(x$cptlist[elorder])




