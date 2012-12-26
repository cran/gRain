extractCPT <- function(x, graph, smooth=0){
  if (!inherits(x, c("data.frame","table")))
    stop("'x' must be a dataframe or a table")      
  if (!inherits(graph, "graphNEL"))
    stop("'graph' must be a graphNEL object")
  if (!is.DAG(graph))
    stop("'graph' must be a DAG")

  V   <- nodes(graph) 
  vpa <- vpar(graph)[V]

  if (class(x)=="data.frame"){
    ans <- lapply(vpa, function(ss){xtabs(~., data=x[, ss, drop=FALSE])})
  } else {
    ans <- lapply(vpa, function(ss){tableMargin(x, ss)})    
  }

  ans <- lapply(ans, as.parray, normalize="first", smooth=smooth)
  ans
}


## FIXME: We also create dag+cptlist here because we need these to be
## able to save the network in Hugin format.
## Not sure whether this should be made here in the future

extractPOT <- function(x, graph, smooth=0){
  if (!inherits(x, c("data.frame","table")))
    stop("'x' must be a dataframe or a table")      
  if (!inherits(graph, "graphNEL"))
    stop("'graph' must be a graphNEL object")
  if (!is.TUG(graph))
    stop("'graph' must be a triangulated undirected graph")

  rr  <- rip(graph)

  if (class(x)=="data.frame"){
    ans <- .extractPotentialDataFrame(x, rr$cliques, rr$sep, smooth=smooth)
  } else {
    ans <- .extractPotentialTable(x, rr$cliques, rr$sep, smooth=smooth)
  }

  dg 	  <- .ug2dag(graph)
  cptlist <- extractCPT(x, dg, smooth=smooth)
  attr(ans, "dag")     <- dg
  attr(ans, "cptlist") <- cptlist
  attr(ans, "rip")     <- rr
  ans 
  
}




.extractPotentialTable <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq    <- cliq[[ii]]
    sp    <- seps[[ii]]
    t.cq  <- tableMargin(x, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp      <- tableMargin(t.cq, sp)
      ans[[ii]] <- tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}

.extractPotentialDataFrame <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq   <- cliq[[ii]]
    sp   <- seps[[ii]]
    xxx  <- xtabs(~., data=x[ , cq, drop=FALSE])
    #xxx  <- as.table(ftable(x[ , cq, drop=FALSE])) 
    t.cq <- tableMargin(xxx, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp      <- tableMargin(t.cq, sp)
      ans[[ii]] <- tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}

## extractCPT <- function(x, graph, V=nodes(graph), smooth=0){
##   UseMethod("extractCPT")
## }


## ## FIXME: Skal bruge is.DAG isf. edgemode
## extractCPT.table <- function(x, graph, V=nodes(graph), smooth=0){
##   if (!identical(edgemode(graph), "directed"))
##     stop("Graph must be directed\n")
##   vpa <- vpar(graph)[V]
  
##   ans <- lapply(vpa, function(s) tableMargin(x, s))

##   ans <- lapply(ans, as.parray, normalize="first", smooth=smooth)
##   return(ans)
## }

## ## FIXME: Skal bruge is.DAG isf. edgemode
## extractCPT.data.frame <- function(x, graph, V=nodes(graph), smooth=0){
##   if (!identical(edgemode(graph), "directed"))
##     stop("Graph must be directed\n")
##   vpa <- vpar(graph)[V]
##   ans <- lapply(vpa,
##                 function(ss){
##                   xtabs(~., data=x[, ss, drop=FALSE])
##                 })
##   ans <- lapply(ans, as.parray, normalize="first", smooth=smooth)
##   return(ans)
## }

## extractPOT <- function(x, graph, smooth=0){
##   UseMethod("extractPOT")
## }

## extractPOT.table <- function(x, graph, smooth=0){

##   if (!identical(edgemode(graph), "undirected"))
##     stop("Graph must be undirected\n")
##   if (length(mcs(graph))==0)
##     stop("Notice: graph is not triangulated\n")
  
##   rr  <- rip(graph)
  
##   ans <- .extractPotentialTable(x, rr$cliques, rr$sep, smooth=smooth)

##   dg 	  <- .ug2dag(graph)
##   cptlist <- extractCPT(x, dg, smooth=smooth)
##   attr(ans, "dag")     <- dg
##   attr(ans, "cptlist") <- cptlist
##   attr(ans, "rip")     <- rr
##   ans 
## }

## extractPOT.data.frame <- function(x, graph, smooth=0){
##   if (!identical(edgemode(graph), "undirected"))
##     stop("Graph must be undirected\n")
##   if (length(mcs(graph))==0)
##     stop("Notice: graph is not triangulated\n")

##   rr  <- rip(graph)

##   ans <- .extractPotentialDataFrame(x, rr$cliques, rr$sep, smooth=smooth)

##   dg 	  <- .ug2dag(graph)
##   cptlist <- extractCPT(x, dg, smooth=smooth)
##   attr(ans, "dag")     <- dg
##   attr(ans, "cptlist") <- cptlist
##   attr(ans, "rip")     <- rr
##   ans 
## }
