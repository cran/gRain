
compile.grain <-
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=object$control, details=0,...) {
  method <- match.arg(tolower(method), c("mcwh","r"))
  NextMethod("compile") 
}


"compile.cpt-grain" <- 
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=object$control, details=0,...){
    t00 <-  proc.time()
    
    ##cat("compile.cpt-grain\n")
    ## FIXME: Do we need elimination order???
    ##
    t0 <- proc.time()
    elorder <- .eliminationOrder(object$dag)  
    if (!is.null(control$timing) && control$timing)
      cat("Time: elimination order:", proc.time()-t0,"\n")
    
    ## Moralize DAG
    ##
    t0   <- proc.time()
    mdag <- moralize(object$dag)
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
    t0    <- proc.time()
    vn    <- nodes(mdag)
    nlev  <- object$universe$nlev[vn]
    ug    <- triangulate(mdag, method=method, nLevels=nlev)
    rip   <- rip(ug, nLevels = nlev)
    if (!is.null(control$timing) && control$timing)
      cat("Time: triangulate:", proc.time()-t0,"\n")
    
    ## Insert potentials
    ##  
    t0 <- proc.time()
    pot.with.1   <- .defaultPotentialList(rip,object$universe)     
    origCQpot    <- tempCQpot <- .insertCpt(object$cptlist, pot.with.1, rip, details)
    equilCQpot   <- .insertNA(pot.with.1)
    
    if (!is.null(control$timing) && control$timing)
      cat("Time: Insert cpt into potentials:", proc.time()-t0,"\n") 
    
    ## Add junction tree to object
    ##
    jt <- .createJTreeGraph(rip)
    
    ## Collect results
    ##
    ans      <- list(rip         = rip,
                     jt          = jt,
                     ug          = ug,
                     equilCQpot  = equilCQpot,
                     tempCQpot   = tempCQpot,
                     origCQpot   = origCQpot, 
                     mdag        = mdag,
                     elorder     = elorder,
                     details     = details )
    
    ans        <- c(object, ans)
    class(ans) <- class(object)
    
    ## Total time
    if (!is.null(control$timing) && control$timing)
      cat("Time: (total) compile:", proc.time()-t00,"\n")
    
    ans$isCompiled <- TRUE
    ## Propagate if asked to
    if (propagate){
      .infoPrint(details, 1, cat (".Initializing network\n"))
      ans             <- propagate(ans)
    }  
    return(ans)
  }

"compile.pot-grain" <-
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=object$control, details=0,...) {
    t00 <-  proc.time()

    ## NOTICE: the compiled object will contain a dag and a cptlist.
    ## These are not used for any calculations; only used for saving
    ## the network in Hugin format...
    
    ## Add junction tree to object
    jt <- .createJTreeGraph(object$rip)

    ## Collect results
    ans      <- list(jt          = jt,
                     tempCQpot   = object$equilCQpot,
                     origCQpot   = object$equilCQpot, 
                     mdag        = object$ug,
                     elorder     = mcs(object$ug),
                     details     = details )
    
    object$details <- NULL
    ans            <- c(object, ans)
    ans$isCompiled <- TRUE
    class(ans)     <- class(object)
    
    ## Total time
    if (!is.null(control$timing) && control$timing)
      cat("Time: (total) compile:", proc.time()-t00,"\n")
    
    ## Propagate if asked to
    if (propagate){
      .infoPrint(details, 1, cat(".Initializing network\n"))
      ans             <- propagate(ans)
    }  
    return(ans)
  }



.createJTreeGraph <- function(rip){
  if (length(rip$cliques)>1){
    ft <-cbind(rip$parents, 1:length(rip$parents))
    ft <- ft[ft[,1]!=0,, drop=FALSE]
    V <- seq_along(rip$parents)
    if (nrow(ft)==0){
      jt <- new("graphNEL", nodes = as.character(V), edgemode = "undirected")
    } else {
      jt <- ftM2graphNEL(ft, V=as.character(V), edgemode="undirected")
    }
  } else {
    jt <- new("graphNEL", nodes = "1", edgemode = "undirected")
  }
  return(jt)
}

.insertNA <- function(list.of.tables){
  lapply(list.of.tables,
         function(xxx){
           xxx[] <- NA
           xxx})
}






























