## FIXME: Der mangler at blive lavet DAG og cptlist i compile() for modeller baseret på
## potentials; ellers kan de ikke saves i Hugin format..

compile.grain <- function(object, 
                           method="mcwh", propagate=FALSE, root=NULL, smooth=0,
                           control=object$control, trace=0,...)
{
  method <- match.arg(tolower(method), c("mcwh","r"))

  NextMethod("compile")
  
}

"compile.cpt-grain" <- 
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=x$control, trace=0,...)
{
  t00 <-  proc.time()

  x <- object ## FIXME
  ##cat("compile.cpt-grain\n")
  ## FIXME: Do we need elimination order???
  ##
  t0 <- proc.time()
  elorder    <- .eliminationOrder(x$dag)  
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
  t0   <- proc.time()
  vn   <- nodes(mdag)
  nlev <- x$universe$nlev[vn]
  ug   <- triangulate(mdag, method=method, nLevels=nlev)
  rip  <- rip(ug, nLevels = nlev)
  if (!is.null(control$timing) && control$timing)
    cat("Time: triangulate:", proc.time()-t0,"\n")

  ## Insert potentials
  ##  
  t0 <- proc.time()
  dummypotlist <- .defaultPotentialList(rip,x$universe)     ##gmData
  potlist     <-  potlistwork <- potlistorig <- .insertCpt(x$cptlist, dummypotlist, rip, trace)
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
                   potlist     = potlist,
                   potlistwork = potlistwork,
                   potlistorig = potlistorig, 
                   mdag        = mdag,
                   elorder     = elorder,
                   trace       = trace
                   )

  ans        <- c(x, ans)
  class(ans) <- class(x)

  ## Total time
  if (!is.null(control$timing) && control$timing)
    cat("Time: (total) compile:", proc.time()-t00,"\n")

  ans$compiled <- TRUE
  ## Propagate if asked to
  ##
  if (propagate){
    if (trace>=1) cat (".Initializing network\n")
    ans             <- propagate(ans)
  }  
  return(ans)
}

"compile.pot-grain" <-
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=x$control, trace=0,...)
{
  x <- object ## FIXME
  t00 <-  proc.time()

  rip <- rip(x$ug)
  dg <- .ug2dag(x$ug)
  cptlist <- extractCPT(x$data, dg) 
  
  ## Add junction tree to object
  ##
  jt <- .createJTreeGraph(rip)

  ## Collect results
  ##
  ans      <- list(
                   rip         = rip,
                   jt          = jt,
                   ug          = x$ug,
                   cptlist     = cptlist,
                   dag         = dg,
                   potlist     = x$potlist,
                   potlistwork = x$potlist,
                   potlistorig = x$potlist, 
                   mdag        = x$ug,
                   elorder     = mcs(x$ug),
                   trace       = trace
                   )
  
  ans        <- c(x, ans)
  class(ans) <- class(x)

  ## Total time
  if (!is.null(control$timing) && control$timing)
    cat("Time: (total) compile:", proc.time()-t00,"\n")

  ans$compiled <- TRUE
  ## Propagate if asked to
  ##
  if (propagate){
    if (trace>=1) cat (".Initializing network\n")
    ans             <- propagate(ans)
  }  
  return(ans)
 }



.createJTreeGraph <- function(rip){

  r <- rip
  if (length(r$cliques)>1){
    ft <-cbind(r$parents, 1:length(r$parents))
    ft <- ft[ft[,1]!=0,, drop=FALSE]
    V <- seq_along(r$parents)
    if (nrow(ft)==0){
      jt <- new("graphNEL", nodes = as.character(V), edgemode = "directed")
    } else {
      jt <- ftM2graphNEL(ft, V=as.character(V), edgemode="directed")
    }
  } else {
    jt <- new("graphNEL", nodes = "1", edgemode = "directed")
  }
  return(jt)
}



































## "comp.dag-grain" <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##            control=x$control, trace=0)
## {
##   object$cptlist   <- dag2cptspec(object$dag, object$gmData, smooth=smooth)  ##gmData
##   ans <- "comp.cpt-grain"(object, method=method, propagate=propagate, root=root, smooth=smooth, control=control, trace=trace)      
##   return(ans)
## }

## "compile.dag-grain" <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##            control=x$control, trace=0)
## {
##   cpt <- cptspec(extractCPT(dd, x$dag, smooth=smooth))

  
##   object$cptlist   <- dag2cptspec(object$dag, object$gmData, smooth=smooth)  ##gmData
##   ans <- "comp.cpt-grain"(object, method=method, propagate=propagate, root=root, smooth=smooth, control=control, trace=trace)      
##   return(ans)
## }








## "comp.ug-grain" <-
##   function(x, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
##            control=x$control, trace=0)
## {

##   t00 <-  proc.time()
  
##   ## Check if graph is triangulated
##   ##
##   if (length(mcsMAT(as.adjMAT(x$ug)))==0){
##     cat("Undirected graph is not triangulated...\n"); return(NULL)
##   }

##   ## Just info
##   ##
##   if (!is.null(root))
##     cat("Specifying 'root' has no effect on models defined from an undirected graph\n")

##   ## Triangulate graph and create rooted junction tree
##   ##
##   t0   <- proc.time()
##   vn   <- nodes(x$ug)
##   nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)] ##gmData
##   rip  <- rip(x$ug, nLevels = nLevels)

##   #rip  <- jTree(x$ug, method=method, nLevels=nLev,  control=control)
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: triangulate:", proc.time()-t0,"\n")

##   ## Extract clique potentials from data
##   ##
##   t0 <- proc.time()
##   ##potlist <- potlistwork <- potlistorig <- ug2potspec(x$ug, x$gmData, rip, smooth=smooth)
##   potlist <- potlistwork <- potlistorig <- lapply(.extractPotentialTable(observations(x$gmData), rip$cliq, rip$sepa, smooth=smooth), as.ptable) ## FIXME : Check at dette er rigtigt
##   ## gmData
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: create potentials:", proc.time()-t0,"\n")

##   ## Create dag and cpts
##   ##
##   dg <- ug2dag(x$ug)
##   cptlist <- dag2cptspec(dg, x$gmData, smooth=smooth) ## gmData

##   ## Collect results
##   ##  
##   ans <- list(dag         = dg,
##               mdag        = x$ug,
##               cptlist     = cptlist,
##               rip         = rip,
##               ##ug          = x$ug,
##               potlist     = potlist,
##               potlistwork = potlistwork,
##               potlistorig = potlistorig,
##               trace       = trace
##               )
##   ans <- c(x, ans)
##   class(ans) <- class(x)
##   ##class(ans) <- c('compgrain', class(x))

##   ## Total time
##   ##
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: (total) compile:", proc.time()-t00,"\n")

##   ## Done
##   ##  
##   return(ans)
## }







  
##   t0 <- proc.time()
##   vn   <- nodes(mdag)
##   nLev <- x$gmData$nLevels[match(vn,x$gmData$varNames)]  ##gmData
##   ug   <- triangulate(mdag, method=method, nLevels=nLev)
##   rip  <- rip(ug, nLevels = nLevels)
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: triangulate:", proc.time()-t0,"\n")
  
##   ## Create initial list of potentials (all with 1's in all cells)
##   ##
##   t0 <- proc.time()
##   dummypotlist <- .createPotentialList(rip,x$gmData)     ##gmData
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: Create potentials:", proc.time()-t0,"\n")

##   ## Insert CPT's in potential list
##   t0 <- proc.time()
##   potlist     <-  potlistwork <- potlistorig <- .insertCpt(x$cptlist, dummypotlist, rip, trace)
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: Insert cpt into potentials:", proc.time()-t0,"\n") 
  
  
##   ## Collect results
##   ##
##   ans      <- list(
##                    rip         = rip,
##                    ug          = ug,
##                    potlist     = potlist,
##                    potlistwork = potlistwork,
##                    potlistorig = potlistorig, 
##                    mdag        = mdag,
##                    elorder     = elorder,
##                    trace       = trace
##                    )
  
##   ans        <- c(x, ans)
##   class(ans) <- class(x)

##   ## Total time
##   if (!is.null(control$timing) && control$timing)
##     cat("Time: (total) compile:", proc.time()-t00,"\n")

##   return(ans)
