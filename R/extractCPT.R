extractCPT <- function(x, graph, V=nodes(graph), smooth=0){
  UseMethod("extractCPT")
}

extractPOT <- function(x, graph, smooth=0){
  UseMethod("extractPOT")
}

extractCPT.table <- function(x, graph, V=nodes(graph), smooth=0){
  if (!identical(edgemode(graph), "directed"))
    stop("Graph must be directed\n")
  vpa <- vpar(graph)[V]
  ans <- lapply(vpa, function(s) tableMargin(x, s))
  ans <- lapply(ans, as.ptable, normalize="first", smooth=smooth)
  return(ans)
}

extractCPT.data.frame <- function(x, graph, V=nodes(graph), smooth=0){
  if (!identical(edgemode(graph), "directed"))
    stop("Graph must be directed\n")
  vpa <- vpar(graph)[V]
  ans <- lapply(vpa,
                function(s){
                  xtabs(~., data=x[,s,drop=FALSE])
                }
                )
  ans <- lapply(ans, as.ptable, normalize="first", smooth=smooth)
  return(ans)
}

extractPOT.table <- function(x, graph, smooth=0){
  if (!identical(edgemode(graph), "undirected"))
    stop("Graph must be undirected\n")
  if (length(mcs(graph))==0)
    cat("Notice: graph is not triangulated\n")

  r <- rip(graph)
  .extractPotentialTable(x, r$cliques, r$sep, smooth=smooth)
}


extractPOT.data.frame <- function(x, graph, smooth=0){
  if (!identical(edgemode(graph), "undirected"))
    stop("Graph must be undirected\n")
  if (length(mcs(graph))==0)
    cat("Notice: graph is not triangulated\n")

  r <- rip(graph)
  .extractPotentialDataFrame(x, r$cliques, r$sep, smooth=smooth)
}


### FIXME: Not sure if smoothing this way is entirely appropriate.
###
.extractPotentialTable <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq <- cliq[[ii]]
    sp <- seps[[ii]]
    t.cq <- tableMargin(x, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp <- tableMargin(t.cq, sp)
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
    cq <- cliq[[ii]]
    sp <- seps[[ii]]
    xxx <- xtabs(~., data=x[,cq,drop=FALSE])
    t.cq <- tableMargin(xxx, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp <- tableMargin(t.cq, sp)
      ans[[ii]] <- tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}







## extractPotentials.data.frame <- function(x, cliq, seps=NULL, smooth=0){
##   ans <- vector("list", length(cliq))
##   for (ii in seq_along(cliq)){
##     cq <- cliq[[ii]]
##     sp <- seps[[ii]]
##     t.cq <- table(x[,cq]) + smooth
##     names(dimnames(t.cq)) <- cq
##     if (!is.null(seps) && length(sp)>0){
##       t.sp <- tableMargin(t.cq, sp)
##       ans[[ii]] <- .tableOp2(t.cq, t.sp, op=`/`)
##     } else {
##       ans[[ii]] <- t.cq / sum(t.cq)
##     }
##   }
##   ans
## }
