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
  
  if (!object$compiled){
    #cat("Compiling (and propagating) model ...\n")
    object <- compile(object, propagate=TRUE)
  } else {
    if (!object$propagated){
      #cat("Propagating model...\n")
      object <- propagate(object)
    }
  }

  type = match.arg(type, choices=c("marginal","joint","conditional"))
  switch(type,
         "marginal"={
           ans <- nodeMarginal(object, nodes, details)
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
           ans     <- ptable(nodes, nst, values=qobject$Freq, normalize="first") ## BRIS
           
           if (return=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         })
  if (object$control$timing)
    cat("Time: query", proc.time()-t0, "\n")
  
  ans
}




nodeJoint <- function(bn, set=NULL, normalize=TRUE,details=1){

  if (is.null(set))
    set <- bn$rip$nodes

  cli  <- bn$rip$cliques

  idxb <- sapply(cli, function(d) subsetof(set, d))
  
  if (any(idxb)){
    if (details>=1) cat(".Calculating directly from clique\n")
    tab <- bn$potlist[[which(idxb)[1]]]
    value <- tableMargin(tab, set)
    if (!normalize){
      value$values <- value$values * pFinding(bn)
    }
  } else {
    vl    <- bn$universe$levels[set]
    value <- ptable(names(vl),vl)
    levs  <- as.data.frame.table(value)[,1:length(vl), drop=FALSE] ## BRIS
    levs2 <- do.call("cbind",lapply(levs, as.character))
    p <- sapply(1:nrow(levs2), function(i){
      #cat ("nodeJoint:","\n")
      #print(levs2[i,])
      pFinding(setFinding(bn, nodes=set, states=levs2[i,]))
    })
    if (normalize)
      p <- p / sum(p)
    attributes(p) <- attributes(value)
    value <- p
  }
  return(value)
}  


nodeMarginal <- function(x, set=NULL,details=1){

  potlist  <- x$potlist
  rip      <- x$rip
  netnodes <- rip$nodes
  
  if (is.null(set))
    nodes  <- netnodes
  else
    nodes  <- set

  nodes <- intersect(netnodes, nodes)
  nodes <- setdiff(nodes, getFinding(x)$nodes)

  if (length(nodes)>0){
    cli    <- rip$cliques
    mtablist <- as.list(rep(NA, length(nodes)))
    for (i in 1:length(nodes)){
      cvert  <- nodes[i]
      idxall <- which(sapply(cli, function(x) subsetof(cvert,x)))
      idx    <- idxall[1]
      cpot   <- potlist[[idx]]
      ##print(cpot)
      mtab   <- tableMargin(cpot, cvert)
      mtab   <- mtab/sum(mtab)
      mtablist[[i]] <- mtab
    }
    names(mtablist) <- nodes
    return(mtablist)
  } 
}

## print.ctabnumeric <- function(x,...){
##   cat(paste(attr(x,"varNames"),collapse=' '),"\n")
##   print.default(c(x))
##   invisible(x)
## }












