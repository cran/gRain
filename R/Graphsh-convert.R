convertg <- function(gg, to=c("NEL","graphsh","matrix")) {
  to <- match.arg(to, c("NEL","graphsh","matrix"))
  switch(to,
         "NEL"=    {.convert2NEL(gg)    },
         "graphsh"={.convert2graphsh(gg)},
         "matrix"= {.convert2matrix(gg) }
         )
}

.adjmat2graphsh <- function(amat){
  is.ug <-  identical(t(amat),amat)
  vn    <- colnames(amat)
  
  if (is.ug){
    amat  <- upper.tri(amat) & amat
    ii    <- which(amat, arr.ind=TRUE)
    amat  <- matrix(vn[ii],nc=2)
    new("ugsh", nodes=vn, edges=split(amat, row(amat)))
  } else {
    amat  <- t(amat)
    ii    <- which(amat, arr.ind=TRUE)
    amat  <- matrix(vn[ii],nc=2)
    new("dagsh", nodes=vn, edges=split(amat, row(amat)))
  }
}

.graphsh2adjmat <- function(gg){
  adjmat(gg)
}

.convert2matrix <- function(gg){
  if (inherits(gg,"ugsh") | inherits(gg,"dagsh")){
    adjmat(gg)
  } else {
    if (inherits(gg,"graphNEL")){
      .graphsh2adjmat(.convert2graphsh(gg))
    } else {
      gg
    }
  }
}

.convert2NEL <- function(gg){
  if (inherits(gg,"matrix"))
    return(.convert2NEL(.adjmat2graphsh(gg)))


  if (inherits(gg,"ugsh")){
    edgemode <- "undirected"
  } else {
    edgemode <- "directed"  
  }
  amat <- adjmat(gg)
  V    <- colnames(amat)
  edL <- vector("list", length = length(V))
  names(edL) <- V
  for (i in 1:length(V)){
    edL[[i]] <- list(edges=which(amat[i,]))
  }
  gR <- new("graphNEL", nodes = V, edgeL = edL,edgemode = edgemode)
  gR
}


.convert2graphsh <- function(gg){
  if (inherits(gg,"matrix"))
    return(.adjmat2graphsh(gg))
      
  switch(edgemode(gg),
    "directed"={
      ed  <- edges(gg)
      ed  <- ed[sapply(ed,length)>0]
      ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), ed,names(ed))
      ed2 <- maximalSet(unlist(ed2,recursive=FALSE))
      gg  <- new("dagsh", nodes=nodes(gg), edges=ed2)
    }, 
    "undirected"={
      ed  <- edges(gg)
      ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), names(ed),ed,SIMPLIFY=FALSE)
      #print("JJJJJJJJJJJJJJJJJJJJJJJ")
      #print(ed2)
      #ed2<<- ed2
      ed2 <- maximalSet(unlist(ed2,recursive=FALSE))
      ##print(ed2)
      ed2 <- ed2[sapply(ed2, length)==2]
      gg  <- new("ugsh", nodes=nodes(gg), edges=ed2)
    })
    return(gg)
}

