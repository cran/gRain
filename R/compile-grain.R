## FIXME: Der mangler at blive lavet DAG og cptlist i compile() for modeller baseret på
## potentials; ellers kan de ikke saves i Hugin format..

compile.grain <- function(object, 
                           method="mcwh", propagate=FALSE, root=NULL, smooth=0,
                           control=object$control, details=0,...)
{
  method <- match.arg(tolower(method), c("mcwh","r"))

  NextMethod("compile")
  
}

"compile.cpt-grain" <- 
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=x$control, details=0,...)
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
  potlist     <-  potlistwork <- potlistorig <- .insertCpt(x$cptlist, dummypotlist, rip, details)
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
                   details       = details
                   )

  ans        <- c(x, ans)
  class(ans) <- class(x)

  ## Total time
  if (!is.null(control$timing) && control$timing)
    cat("Time: (total) compile:", proc.time()-t00,"\n")

  ans$isCompiled <- TRUE
  ## Propagate if asked to
  ##
  if (propagate){
    .infoPrint(details, 1, cat (".Initializing network\n"))
    ans             <- propagate(ans)
  }  
  return(ans)
}

"compile.pot-grain" <-
  function(object, method="mcwh", propagate=FALSE, root=NULL, smooth=0,
           control=x$control, details=0,...)
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
                   details       = details
                   )
  
  ans        <- c(x, ans)
  class(ans) <- class(x)

  ## Total time
  if (!is.null(control$timing) && control$timing)
    cat("Time: (total) compile:", proc.time()-t00,"\n")

  ans$isCompiled <- TRUE
  ## Propagate if asked to
  ##
  if (propagate){
    .infoPrint(details, 1, cat(".Initializing network\n"))
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


































