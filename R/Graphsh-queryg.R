queryg <-function(gg, type, set=NULL, set2=NULL, set3=NULL){
  type=match.arg(type, 
    choices=c(
      "maxClique","cliques", 
      "connectedComp","concomp",
      "separates",
      "nodes",
      "edges",
      "adj",
      "cl",
      "pa",
      "ch",
      "ne",
      "an",
      "is.complete",
      "simplicialNodes",
      "is.simplicial",
      "ancestralSet",
      "ancestralGraph",
      "is.triangulated",
      "subgraph"
      ))
  is.graphsh <- inherits(gg,"ugsh") || inherits(gg,"dagsh")
  switch(type,
         "maxClique"=,"cliques"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           maxClique(gg)$maxCliques
         },
         "connectedComp"=,"concomp"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           connectedComp(gg)
         },
         "separates"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           separates(set, set2, set3, gg)
         },
         "nodes"={
           nodes(gg)
         },
         "edges"={
           edges(gg)
         },
         "adj"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           adj(gg, set)
         },
         "cl"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           c(set, unlist(adj(gg, set)))
         },
         "pa"={
           x <- adjmat(gg)[,set,drop=FALSE]
           x <- rep(rownames(x), ncol(x))[which(x)]
           x <- setdiff(unique(x),set)
           if (length(x))x else NULL
         },
         "ch"={
           x <- adjmat(gg)[set,,drop=FALSE]
           x <- rep(colnames(x), each=nrow(x))[which(x)]
           x <- setdiff(unique(x),set)
           if (length(x))x else NULL           
         },
         "ne"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           x   <-  adj(gg, set)
           setdiff(unique(unlist(x)),set)
         },
         "an"={
           set <-  c("ve","al")
           An <- set
           x <- adjmat(gg)
           x <- x[-match(set, rownames(x)),]
           
           repeat{
             set2 <- rowSums(x[,set, drop=FALSE])
             set <- names(which(set2>0))
             if (!length(set))
               break()
             An <- c(An, set)
             x <- x[set2 == 0,,drop=FALSE]
           }
           An
         },
         "is.complete"={
           isComplete(gg)
         },
         "simplicialNodes"={
           simplicialNodes(gg)
         },
         "is.simplicial"={
           isSimplicial(gg, set)
         },
         "ancestralSet"={
           .ancestralSet(gg, set)
         },
         "ancestralGraph"={
           ancestralGraph(gg, set)
         },
         "is.triangulated"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           is.triangulated(gg)
         },
         "subgraph"={
           if (is.graphsh)
             gg <- convertg(gg, "NEL")
           convertg(subGraph(set, gg),to="graphsh")
         }         
         )  
}


ancestralGraph <- function(dag, set){
  A<-.ancestralSet(dag, set)
  g<-convertg(dag, "NEL")
  sg<-subGraph(A,g) 
  sg<-convertg(sg, "graphsh")
  sg
  }

.ancestralSet <- function(dag, set){
  amat <- adjmat(dag)
  vn <- colnames(amat)
  an <- rep(0, length(vn))
  names(an) <- vn
  an[set] <- 1
  
  A0 <- set
  repeat{
    x <- amat[,A0,drop=FALSE]
    B <- rownames(x)[apply(x,1,sum)>0]
    if (!length(B))
      break()
    an[B] <- 1
    idx <- match(A0, colnames(amat))
    amat <- amat[-idx,-idx,drop=FALSE]
    vn <- colnames(amat)
    A0 <- intersect(B,vn)
    if (!length(A0))
      break()
  }
  names(an[an>0])
}


