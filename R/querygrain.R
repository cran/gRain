querygrain <- function(object,nodes=nodeNames(object), normalize=TRUE,
                    type="marginal", result="array", details=0)
{
  UseMethod("querygrain")
}

querygrain.grain <- function(object, nodes=nodeNames(object), normalize=TRUE,
                               type="marginal", result="array", details=0){

  type <- match.arg(type, c("marginal","joint","conditional"))
  result <- match.arg(result, c("array","data.frame"))
  t0 <- proc.time()

  if (is.null(nodes))
    return(invisible(NULL))

  if (!object$isCompiled){
    if (details>=1) cat("  Compiling (and propagating) model ...\n")
    object <- compile(object, propagate=TRUE)
  } else {
    if (!object$isPropagated){
      if (details>=1) cat("  Propagating model...\n")
      object <- propagate(object)
    }
  }

  type = match.arg(type, choices=c("marginal","joint","conditional"))
  switch(type,
         "marginal"={
           ans <- .nodeMarginal(object, nodes, details) #;print(ans)
           if (result=="data.frame")
             ans <- lapply(ans, as.data.frame.table)
         },
         "joint"={
           ans<-.nodeJoint(object, nodes, normalize, details)
           if (result=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         },
         "conditional"={
           qobject <- querygrain(object, nodes, type="joint", result="data.frame")
           nst     <- nodeStates(object)[nodes]
           ans     <- parray(nodes, nst, values=qobject$Freq, normalize="first") ## BRIS
           if (result=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         })
  if (object$control$timing)
    cat("Time: query", proc.time()-t0, "\n")
  ans
}


.nodeJoint <- function(object, set=NULL, normalize=TRUE,details=1){

  if (is.null(set))
    set <- object$rip$nodes
  cli  <- object$rip$cliques
  idxb <- sapply(cli, function(d) subsetof(set, d))

  if (any(idxb)){
    .infoPrint(details,1, cat(".Calculating directly from clique\n"))
    ## querygrain - .nodeJoin: Calculations based on equilCQpot
    tab <- object$equilCQpot[[which(idxb)[1]]]
    value <- tableMargin(tab, set)
    if (!normalize){
      value$values <- value$values * pFinding(object)
    }
  } else {
    vl    <- object$universe$levels[set]
    value <- parray(names(vl),vl)
    levs  <- as.data.frame.table(value)[,1:length(vl), drop=FALSE] ## BRIS
    levs2 <- do.call("cbind",lapply(levs, as.character))
    p <- sapply(1:nrow(levs2), function(i){
      #cat ("nodeJoint:","\n")
      #print(levs2[i,])
      pFinding(setFinding(object, nodes=set, states=levs2[i,]))
    })
    if (normalize)
      p <- p / sum(p)
    attributes(p) <- attributes(value)
    value <- p
  }
  return(value)
}


.nodeMarginal <- function(object, set=NULL,details=1){

  .get_host2 <- function(cvert, cli) which(isin(cli, cvert, index=TRUE)>0)[1]
  #cat("CHK: .nodeMarginal\n")
  ## querygrain - .nodeMarginal: Calculations based on equilCQpot
  equilCQpot  <- object$equilCQpot
  .rip      <- object$rip
  netnodes <- .rip$nodes

  if (is.null(set))
    nodes  <- netnodes
  else
    nodes  <- set

  nodes <- intersect(netnodes, nodes)
  nodes <- setdiff(nodes, getFinding(object)$nodes)

  if (length(nodes)>0){
    vn   <- .rip$nodes
    ## host <- .getHost(.rip) ## Obsolete; replaced by
    host <- .rip$host

    mtablist <- vector("list",length(nodes))
    names(mtablist) <- nodes

    for (i in 1:length(nodes)){
      cvert  <- nodes[i]
      ##       idx <- which(sapply(cli, function(x) subsetof(cvert,x)))[1]
      ##      idx <- .get_host2(cvert, cli)
      idx <- host[ match( cvert, vn ) ]

      ## querygrain - .nodeMarginal: Calculations based on equilCQpot
      cpot   <- equilCQpot[[ idx ]]
      mtab   <- tableMargin( cpot, cvert )
      mtab   <- mtab/sum( mtab )
      mtablist[[i]] <- mtab
    }
    return(mtablist)
  }
}












