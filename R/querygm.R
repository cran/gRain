querygm <- function(object,nodes=nodeNames(object), normalize=TRUE,
                    type=c("marginal","joint","conditional"),
                    return="array",
                    trace=0)
{
  UseMethod("querygm")
}

querygm.gmInstance <- function(object, nodes=nodeNames(object), normalize=TRUE,
                               type=c("marginal","joint","conditional"),
                               return="array",
                               trace=0){

  return <- match.arg(return, c("array","data.frame"))
  t0 <- proc.time()
  if (!inherits(object, "compgmInstance")){
    #cat("Compiling model...\n")
    object <- compilegm(object)
  }


  
  if (!object$initialized){
    #cat("Propagating model...\n")
    object <- propagate(object)
  }
  if (is.null(nodes))
    return(invisible(NULL))


  type = match.arg(type, choices=c("marginal","joint","conditional"))
  switch(type,
         "marginal"={
           ans <- nodeMarginal(object, nodes, trace)
           ans <- ans[nodes]
           if (return=="data.frame")
             ans <- lapply(ans, as.data.frame.table)
         },
         "joint"={
           ans<-nodeJoint(object, nodes, normalize, trace)
           ## ans <- as.data.frame(ans)
           if (return=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         },
         "conditional"={
           qobject <- querygm(object, nodes, type="joint", return="data.frame")
           nst     <- nodeStates(object)[nodes]
           ##ans     <- ctab(nodes, nst, values=qobject$values, normalize="first")
           ans     <- ptable(nodes, nst, values=qobject$Freq, normalize="first") ## BRIS
           
           ##ans <- as.data.frame(ans)
           if (return=="data.frame")
             ans <- as.data.frame.table(ans) ## BRIS
         })
  if (object$control$timing)
    cat("Time: query", proc.time()-t0, "\n")
  
  ans
}
  
                   
nodeJoint <- function(bn, set=NULL, normalize=TRUE,trace=0){

  if (is.null(set))
    set <- bn$rip$nodes

  cli  <- bn$rip$cliques

  idxb <- sapply(cli, function(d) subsetof(set, d))
  
  if (any(idxb)){
    if (trace>=1) cat(".Calculating directly from clique\n")
    tab <- bn$potlist[[which(idxb)[1]]]
    value <- tableMarginPrim(tab, set)
    if (!normalize){
      value$values <- value$values * pevidence(bn)
    }
  } else {
    vl    <- valueLabels(bn$gmData)[set]
    value <- ptable(names(vl),vl)
    levs  <- as.data.frame.table(value)[,1:length(vl), drop=FALSE] ## BRIS
    levs2 <- do.call("cbind",lapply(levs, as.character))
    p<-sapply(1:nrow(levs2), function(i)
              pevidence(enterEvidence(bn, nodes=set, states=levs2[i,]))
              )
    if (normalize)
      p <- p / sum(p)
    attributes(p) <- attributes(value)
    value <- p
  }
  return(value)
}  







nodeMarginal <- function(x, set=NULL,trace=0){

  potlist  <- x$potlist
  rip      <- x$rip
  netnodes <- rip$nodes
  
  if (is.null(set))
    nodes  <- netnodes
  else
    nodes  <- set

  nodes <- intersect(netnodes, nodes)
  nodes <- setdiff(nodes, evidence(x)$nodes)

  if (length(nodes)>0){
    cli    <- rip$cliques
    mtablist <- as.list(rep(NA, length(nodes)))
    for (i in 1:length(nodes)){
      cvert <- nodes[i]
      idxall<-which(sapply(cli, function(x) subsetof(cvert,x)))
      idx <- idxall[1]
      cpot <- potlist[[idx]]
      mtab <- tableMarginPrim(cpot, cvert,normalize=TRUE)
      mtablist[[i]] <- mtab
    }
    names(mtablist) <- nodes
    return(mtablist)
  } 
}

print.ctabnumeric <- function(x,...){
  cat(paste(attr(x,"varNames"),collapse=' '),"\n")
  print.default(c(x))
  invisible(x)
}



# querybn <- function(bn, nodes=nodeNames(bn), normalize=TRUE,
#                     type=c("marginal","joint","conditional"), trace=0){
#   if (!bn$initialized){
#     bn <- propagate(bn)
#     #cat("Network is not propagated - can not answer queries...\n")
#     #return(invisible(NULL))
#   }
#   if (is.null(nodes))
#     return(invisible(NULL))

#   type = match.arg(type)
#   switch(type,
#          "marginal"={
#            nodeMarginal(bn, nodes, trace)
#          },
#          "joint"={
#            nodeJoint(bn, nodes, normalize, trace)
#          },
#          "conditional"={
#            qbn <- querybn(bn, nodes, type="joint")
#            nst <- nodeStates(bn)[nodes]
#            #nst <- sapply(nst, length)
#            ctab(nodes, nst, values=qbn$values, normalize="first")
#          })
# }









