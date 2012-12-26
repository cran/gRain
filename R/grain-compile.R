
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
    
##    cat("compile.cpt-grain\n")
            
## ### Moralize DAG
##     ## Input: dag
##     t0   <- proc.time()
##     mdag <- moralize(object$dag)
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: moralize:", proc.time()-t0,"\n")
        
## ### Add edges between variables in 'root' to the moralized graph    
##     ## Input: mdag
##     if (!is.null(root) && length(root)>1){
##       rootEdges <- combn(root,2)
##       for (jj in 1:ncol(rootEdges)){
##         if (!isAdjacent(mdag, rootEdges[1,jj], rootEdges[2,jj]))
##           mdag <- addEdge(rootEdges[1,jj], rootEdges[2,jj], mdag)
##       }
##     }  
    
## ### Triangulate graph and create rooted junction tree
##     ## Input: mdag, nlev
##     t0    <- proc.time()
##     vn    <- nodes(mdag)
##     nlev  <- object$universe$nlev[vn]
##     ug    <- triangulate(mdag, method=method, nLevels=nlev)
##     rip   <- rip(ug, nLevels = nlev)
##     jt    <- .createJTreeGraph(rip)
##     if (!is.null(control$timing) && control$timing)
##       cat("Time: triangulate:", proc.time()-t0,"\n")

    ## Based on sparse matrices
    t0    <- proc.time()
    mdagM <- moralizeMAT(as(object$dag,"Matrix"))
    vn    <- colnames(mdagM)
    nlev  <- object$universe$nlev[vn]

    if (!is.null(root) && length(root)>1){      
      dn <- dimnames(mdagM)    
      ft <- names2pairs(match(root, vn),sort=FALSE, result="matrix")
      ft <- rbind(ft,ft[,2:1,drop=FALSE])      
      mdagM <- sp_setXtf1(mdagM, ft)
      dimnames(mdagM) <- dn
    }

    ugM   <- triangulateMAT(mdagM)
    rip   <- ripMAT(ugM, nLevels = nlev)
    jt    <- .createJTreeGraph(rip)
    ug    <- as(ugM, "graphNEL")
    mdag  <- as(mdagM, "graphNEL")
    if (!is.null(control$timing) && control$timing)
      cat("Time: triangulate(2):", proc.time()-t0,"\n")
    

### Sparse version of it all
##     mdagM <- moralizeMAT(object$dagM)
##     if (!is.null(root) && length(root)>1){      
##       vn <- object$universe$nodes
##       dn <- dimnames(mdagM)    
##       ft <- names2pairs(match(root, vn),sort=FALSE, result="matrix")
##       ft <- rbind(ft,ft[,2:1,drop=FALSE])      
##       mdagM <- sp_setXtf1(mdagM, ft)
##       dimnames(mdagM) <- dn
##     }



    
### Insert potentials;
    ## Input: rip, universe, cptlist
    t0 <- proc.time()
    pot.with.1   <- .defaultPotentialList(rip, object$universe)     
    origCQpot    <- tempCQpot <- .insertCpt(object$cptlist, pot.with.1, rip, details)
    equilCQpot   <- .insertNA(pot.with.1)    
    if (!is.null(control$timing) && control$timing)
      cat("Time: Insert cpt into potentials:", proc.time()-t0,"\n") 
    
### Collect results
    ans      <- list(rip         = rip,
                     jt          = jt,
                     ug          = ug,
                     equilCQpot  = equilCQpot,
                     tempCQpot   = tempCQpot,
                     origCQpot   = origCQpot, 
                     mdag        = mdag,
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
      ans <- propagate(ans)
    }  
    return(ans)
  }


## NOTICE: the compiled object will contain a dag and a cptlist.
## These are not used for any calculations; only used for saving
## the network in Hugin format...

"compile.pot-grain" <-
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=object$control, details=0,...) {
    t00 <-  proc.time()
    
    
### Create junction tree object
    jt <- .createJTreeGraph(object$rip)
    
### Collect results
    ans      <- list(jt          = jt,
                     tempCQpot   = object$equilCQpot,
                     origCQpot   = object$equilCQpot, 
                     mdag        = object$ug,
                     elorder     = mcs(object$ug),
                     details     = details )

    object$equilCQpot   <- .insertNA(object$equilCQpot)   
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








### FIXME: Do we need elimination order???
    ##    t0 <- proc.time()
    ##     elorder <- .eliminationOrder(object$dag)  
    ##     if (!is.null(control$timing) && control$timing)
    ##       cat("Time: elimination order:", proc.time()-t0,"\n")
    ##elorder <- NULL


##     dg <<- object$dag
##     md <<- mdag

##     mdagM <- moralizeMAT(object$dagM)
##     mdm <<- mdagM
##     re  <<- rootEdges

### Sparse version of it all
##     mdagM <- moralizeMAT(object$dagM)
##     if (!is.null(root) && length(root)>1){      
##       vn <- object$universe$nodes
##       dn <- dimnames(mdagM)    
##       ft <- names2pairs(match(root, vn),sort=FALSE, result="matrix")
##       ft <- rbind(ft,ft[,2:1,drop=FALSE])      
##       mdagM <- sp_setXtf1(mdagM, ft)
##       dimnames(mdagM) <- dn
##     }
##     ugM   <- triangulateMAT(mdagM, nlevels=nlev)
##     ripM  <- ripMAT(ugM, nLevels = nlev)
    ## After this point all we really need is rip, cptlist

