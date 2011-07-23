querygrain <- function(object,nodes=nodeNames(object), normalize=TRUE,
                    type=c("marginal","joint","conditional"),
                    return="array",
                    details=0)
{
  UseMethod("querygrain")
}

querygrain.grain <- function(object, nodes=nodeNames(object), normalize=TRUE,
                               type=c("marginal","joint","conditional"),
                               return="array",
                               details=0){

  return <- match.arg(return, c("array","data.frame"))
  t0 <- proc.time()

  if (is.null(nodes))
    return(invisible(NULL))
  
  if (!object$isCompiled){
    #cat("CHK: Compiling (and propagating) model ...\n")
    object <- compile(object, propagate=TRUE)
  } else {
    if (!object$isPropagated){
      #cat("CHK: Propagating model...\n")
      object <- propagate(object)
    }
  }

  #print(nodes)

  type = match.arg(type, choices=c("marginal","joint","conditional"))
  #print(type)
  switch(type,
         "marginal"={
           ans <- nodeMarginal(object, nodes, details)
           #print(ans)
           if (return=="data.frame")
             ans <- lapply(ans, as.data.frame.table)
         },
         "joint"={
           ans<-nodeJoint(object, nodes, normalize, details)
           if (return=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         },
         "conditional"={
           qobject <- querygrain(object, nodes, type="joint", return="data.frame")
           nst     <- nodeStates(object)[nodes]
           ans     <- parray(nodes, nst, values=qobject$Freq, normalize="first") ## BRIS
           
           if (return=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         })
  if (object$control$timing)
    cat("Time: query", proc.time()-t0, "\n")
  
  ans
}




nodeJoint <- function(object, set=NULL, normalize=TRUE,details=1){

  if (is.null(set))
    set <- object$rip$nodes

  cli  <- object$rip$cliques

  idxb <- sapply(cli, function(d) subsetof(set, d))
  
  if (any(idxb)){
    .infoPrint(details,1, cat(".Calculating directly from clique\n"))
    ## querygrain - nodeJoin: Calculations based on equilCQpot
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


nodeMarginal <- function(object, set=NULL,details=1){

  #cat("CHK: nodeMarginal\n")
  ## querygrain - nodeMarginal: Calculations based on equilCQpot
  equilCQpot  <- object$equilCQpot
  rip      <- object$rip
  netnodes <- rip$nodes
  
  if (is.null(set))
    nodes  <- netnodes
  else
    nodes  <- set

  #print(nodes)
  nodes <- intersect(netnodes, nodes)
  #print(nodes)
  nodes <- setdiff(nodes, getFinding(object)$nodes)


  
  if (length(nodes)>0){
    cli    <- rip$cliques
    mtablist <- as.list(rep(NA, length(nodes)))
    for (i in 1:length(nodes)){
      cvert  <- nodes[i]
      idxall <- which(sapply(cli, function(x) subsetof(cvert,x)))
      idx    <- idxall[1]
      ## querygrain - nodeMarginal: Calculations based on equilCQpot
      cpot   <- equilCQpot[[idx]]
      ##print(cpot)
      mtab   <- tableMargin(cpot, cvert)
      mtab   <- mtab/sum(mtab)
      mtablist[[i]] <- mtab
    }
    names(mtablist) <- nodes
    return(mtablist)
  } 
}












