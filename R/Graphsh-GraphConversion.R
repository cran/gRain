
## Convert grash/graphNEL to adjacency matrix
##
as.adjmat <- function(object, vn=nodes(object)){

  if (!inherits(object,c("dagsh","ugsh","graphNEL"))){
    stop("Can not convert to adjmat...\n")
  }

  if (inherits(object,c("dagsh","ugsh"))){ 
    .grash2adjmat(object,vn=vn) 
  } else {
    .grash2adjmat(.nel2grash(object))
  }
}


## Convert grash/adjmat to graphNEL
##
as.graphNEL <- function(object){

  if (!inherits(object,c("dagsh","ugsh","matrix"))){
    stop("Can not convert to graphNEL...\n")
  }

  if (inherits(object,"matrix")){
    g <- .adjmat2nel(object)
  } else {
    g <- .grash2nel(object)
  }
  return(g)
}

## Convert adjacency matrix/graphNEL to grash
##
as.grash <- function(object){

  if (!inherits(object,c("graphNEL","matrix"))){
    stop("Can not convert to grash...\n")
  }

  if (inherits(object, "graphNEL")){
    .nel2grash(object)
  } else {
    .adjmat2grash(object)
  }
}


### PRIMITIVES BELOW HERE ###

.grash2nel <- function(gg){

  if( inherits(gg,"ugsh") ){
    em <- "undirected"
  } else {
    em <- "directed"
  }
  vn <- nodes(gg)
  ed <- adjmat2nel(pl2adjmat(edges(gg),vn=vn,edgemode=em))
  g  <- new("graphNEL", nodes=vn, edgeL=ed, edgemode=em)
  return(g)
}


.grash2adjmat <- function(gg,vn=nodes(gg)){
  imat  <- matrix(FALSE, nc=length(vn), nr=length(vn))
  dimnames(imat) <- list(vn,vn)
  ed    <- edges(gg)

  if (length(ed)>0){
    for (i in 1:length(ed)){
      curre <- ed[[i]]
      imat[curre[2],curre[1]]<- 1
    }
  }
  if (inherits(gg, "ugsh")){
    imat <- imat + t(imat)
  }
  return(imat)
}


.nel2grash <- function(gg){
  switch(edgemode(gg),
    "directed"={
      ed  <- edges(gg)
      ed  <- ed[sapply(ed,length)>0]
      ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), ed,names(ed),SIMPLIFY=FALSE)
      ed2 <- maximalSet(unlist(ed2,recursive=FALSE))
      gg  <- new("dagsh", nodes=nodes(gg), edges=ed2)
    }, 
    "undirected"={
      ed  <- edges(gg)
      ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), names(ed),ed,SIMPLIFY=FALSE)
      ed2 <- maximalSet(unlist(ed2,recursive=FALSE))
      ed2 <- ed2[sapply(ed2, length)==2]
      gg  <- new("ugsh", nodes=nodes(gg), edges=ed2)
    })
    return(gg)
}



.nel2adjmat <- function(gg){
  .grash2adjmat(.nel2grash(gg))
}

.adjmat2grash <- function(amat){
  is.ug <-  identical(t(amat),amat)
  vn    <- colnames(amat)
  
  if (is.ug){
    amat  <- upper.tri(amat) & amat
    ii    <- which(amat==1, arr.ind=TRUE)
    amat  <- matrix(vn[ii],nc=2)
    new("ugsh", nodes=vn, edges=split(amat, row(amat)))
  } else {
    amat  <- t(amat)
    ii    <- which(amat==1, arr.ind=TRUE)
    amat  <- matrix(vn[ii],nc=2)
    new("dagsh", nodes=vn, edges=split(amat, row(amat)))
  }
}




.adjmat2nel <- function(amat){
  .grash2nel(.adjmat2grash(amat))
}

### OBSOLETE BELOW HERE







# convertg <- function(gg, to=c("NEL","grash","matrix")) {
#   to <- match.arg(to, c("NEL","grash","matrix"))
#   switch(to,
#          "NEL"=    {.convert2NEL(gg)    },
#          "grash"={.convert2grash(gg)},
#          "matrix"= {.convert2matrix(gg) }
#          )
# }

# .convert2grash <- function(gg){
#   if (inherits(gg,"matrix"))
#     return(.adjmat2grash(gg))
     
#   switch(edgemode(gg),
#     "directed"={
#       ed  <- edges(gg)
#       ed  <- ed[sapply(ed,length)>0]
#       ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), ed,names(ed),SIMPLIFY=FALSE)
#       ed2 <- maximalSet(unlist(ed2,recursive=FALSE))
#       gg  <- new("dagsh", nodes=nodes(gg), edges=ed2)
#     }, 
#     "undirected"={
#       ed  <- edges(gg)
#       ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), names(ed),ed,SIMPLIFY=FALSE)
#       ed2 <- maximalSet(unlist(ed2,recursive=FALSE))
#       ed2 <- ed2[sapply(ed2, length)==2]
#       gg  <- new("ugsh", nodes=nodes(gg), edges=ed2)
#     })
#     return(gg)
# }


# .convert2NEL <- function(gg){
#   if (inherits(gg,"matrix"))
#     return(.convert2NEL(.adjmat2grash(gg)))


#   if (inherits(gg,"ugsh")){
#     edgemode <- "undirected"
#   } else {
#     edgemode <- "directed"  
#   }
#   amat <- as.adjmat(gg)
#   V    <- colnames(amat)
#   edL <- vector("list", length = length(V))
#   names(edL) <- V
#   for (i in 1:length(V)){
#     edL[[i]] <- list(edges=(which(amat[i,]>0)))
#   }
#   gR <- new("graphNEL", nodes = V, edgeL = edL,edgemode = edgemode)
#   gR
# }



# .convert2matrix <- function(gg){
#   if (inherits(gg,"ugsh") | inherits(gg,"dagsh")){
#     as.adjmat(gg)
#   } else {
#     if (inherits(gg,"graphNEL")){
#       .grash2adjmat(.convert2grash(gg))
#     } else {
#       gg
#     }
#   }
# }

