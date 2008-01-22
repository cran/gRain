
nodeNames.gmInstance  <- function(x) x$nodes
nodeStates.gmInstance <- function(x, nodes=nodeNames(x)){
  vl<-valueLabels(x$gmData)
  vl[nodes]
}

##
## Creating gmInstance
##

newgmInstance <- function(x, gmData, description="ProbNet",  control=list(),
                          trace=0,...){
  UseMethod("newgmInstance")
}

.defaultControl <- function(){
  list(timing=0)
}

newgmInstance.cptspec <- function(x, gmData, description="ProbNet", control=list(),
                                  trace=0,...){

  
  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con
  
  if(trace>=1) cat(".Creating DAG from cptlist\n")

  t0 <- proc.time()

  vn  <- lapply(x, varNames)
  dag <- newdagsh(vn)

  if (is.null(dag)){
    cat("Graph defined by the cpt's is not acyclical...\n"); return(NULL)
  }

  nodes <- nodes (dag) 

  if (missing(gmData))
    gmData <- as.gmData(x)

  if (!is.null(control$timing) && control$timing)
    cat("Time: Create dag and gmData:", proc.time()-t0,"\n")

  
  ans  <- list(cptlist     = x,
               gmData      = gmData,
               nodes       = nodes,
               description = description,
               dag         = dag,
               control     = control,
               trace       = trace)
  class(ans) <- c("cpt-gmInstance","gmInstance")
  return(ans)
}

newgmInstance.dagsh <- function(x, gmData, description="ProbNet", control=list(), trace=0,...){

  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con
  
  nodes <- nodes (x)
  ans  <- list(dag         = x,
               gmData      = gmData,
               nodes       = nodes,
               description = description,
               control     = control,
               trace       = trace)
  class(ans) <- c("dag-gmInstance","gmInstance")
  return(ans)
}

newgmInstance.ugsh <- function(x, gmData, description="ProbNet", control=list(), trace=0,...){

  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con
  
  nodes      <- nodes (x)
  ans        <- list(ug          = x,
                     gmData      = gmData,
                     nodes       = nodes,
                     description = description,
                     control     = control,
                     trace       = trace)
  class(ans) <- c("ug-gmInstance","gmInstance")
  return(ans)
}


##
## Compiling gmInstance
##

compilegm <-
  function(x, method=c("mcwh","robust"), propagate=FALSE, root=NULL, smooth=0,
            control=x$control,
           trace=0){
  UseMethod("compilegm")
}

"compilegm.dag-gmInstance" <-
  function(x, method=c("mcwh","robust"), propagate=FALSE, root=NULL, smooth=0,
           control=x$control,
           trace=0){
    
    cptlist <- dag2cptspec(x$dag,x$gmData,smooth=smooth)
    x$cptlist <- cptlist
    "compilegm.cpt-gmInstance"(x, method=method, propagate=propagate,
                             root=root, smooth=smooth, trace=trace)
  }


"compilegm.cpt-gmInstance" <-
  function(x, method=c("mcwh","robust"), propagate=FALSE, root=NULL, smooth=0,
           control=x$control,
           trace=0){

    method <- match.arg(method,c("mcwh","robust"))

    t00 <- t0 <- proc.time()
    elorder    <- eliminationOrder(x$dag)
    if (!is.null(control$timing) && control$timing)
      cat("Time: elimination order:", proc.time()-t0,"\n")

    ### UPS - dette virker ikke.... Hvorfor...
    ##x$cptlist  <- cptspec(x$cptlist[elorder])


    t0 <- proc.time()
    mdag       <- moralize(x$dag)
    if (!is.null(control$timing) && control$timing)
      cat("Time: moralize:", proc.time()-t0,"\n")
    
    if (!is.null(root)){
      if (length(root)>1)
        mdag <- updateugsh(mdag, root)
    }

    vn   <- nodes(mdag)
    nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]

    t0 <- proc.time()


    rip <- junctionTree(mdag, method=method, vn=vn, nLevels=nLev,  control=control)
    ##rip <<- rip
    ##print(rip)
    
    if (!is.null(control$timing) && control$timing)
      cat("Time: triangulate:", proc.time()-t0,"\n")

    t0 <- proc.time()

    dummypotlist <- .createPotentialList(rip,x$gmData)
    if (!is.null(control$timing) && control$timing)
      cat("Time: Create potentials:", proc.time()-t0,"\n")


    t0 <- proc.time()
    potlist     <-  .insertCpt(x$cptlist, dummypotlist, rip, trace)
    potlistwork <- potlistorig <- potlist 
    if (!is.null(control$timing) && control$timing)
      cat("Time: Insert cpt into potentials:", proc.time()-t0,"\n") 

    ans      <- list(rip         = rip,
                     potlist     = potlist,
                     potlistwork = potlistwork,
                     potlistorig = potlistorig, 
                     mdag        = mdag,
                     elorder     = elorder,
                     trace       = trace,
                     initialized = FALSE,
                     propagated  = FALSE)
    
    ans        <- c(x, ans)
    class(ans) <- c('compgmInstance', class(x))
    
    if (propagate){
      if (trace>=1) cat (".Initializing network\n")
      ans             <- propagate(ans)
    }

    if (!is.null(control$timing) && control$timing)
      cat("Time: (total) compile:", proc.time()-t00,"\n")

    return(ans)

  }


"compilegm.ug-gmInstance" <-
  function(x, method=c("mcwh","robust"), propagate=FALSE, root=NULL, smooth=0,
           control=x$control,
           trace=0){

    method <- match.arg(method,c("mcwh","robust"))
    if (is.null(mcs(x$ug))){
      cat("Undirected graph is not triangulated...\n"); return(NULL)
    }

    if (!is.null(root)){
      cat("Specifying 'root' has no effect on models defined from an undirected graph\n")
     }


    t0 <- proc.time()
    vn   <- nodes(x$ug)
    nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]
    rip  <- junctionTree(x$ug, method, vn, nLev, control)
    rip <<- rip

    if (!is.null(control$timing) && control$timing)
      cat("Time: triangulate:", proc.time()-t0,"\n")


    t0 <- proc.time()
    potlist <- potlistwork <- potlistorig <- ug2potspec(x$ug, x$gmData, rip, smooth=smooth)
    if (!is.null(control$timing) && control$timing)
      cat("Time: create potentials:", proc.time()-t0,"\n")

    
    ans <- list(rip         = rip,
                potlist     = potlist,
                potlistwork = potlistwork,
                potlistorig = potlistorig,
                initialized = FALSE,
                propagated  = FALSE
                )
    
    ans <- c(x, ans)
    class(ans) <- c('compgmInstance', class(x))
    
    if (propagate){
      if (trace>=1) cat (".Initializing network\n")
      ans             <- propagate(ans)
    }
    return(ans)
  }


## Printing gmInstance
##

print.gmInstance <- function(x,...){
  cat("Probabilistic network:", x$description, " ")

  isCompiled <- inherits(x, "compgmInstance")
  isPropagated <- x$propagated
  if (is.null(isPropagated))
    isPropagated <- FALSE
  cat("Compiled:", isCompiled, "Propagated:",
      isPropagated, "\n")

  ##  cat("Nodes :", x$nodes,"\n")

  #if (isCompiled){
  #  cat("Model is propagated:", isPropagated, "\n")
  #}
  return(invisible(x))
}
