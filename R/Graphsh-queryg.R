queryg <-function(object, type, set=NULL, set2=NULL, set3=NULL){

  if (!inherits(object,c("ugsh","dagsh")))
    stop("queryg needs an ugsh-graph or a dagsh-graph\n")
  
  type=match.arg(type,  choices=c( 
                          ## From graph package
                          ##
                          "maxClique","cliques", 
                          "connectedComp","concomp",
                          "separates",
                          "adj",
                          "cl",
                          "ne",
                          "is.triangulated",
                          "subgraph",
                          ## SHD functions
                          ##
                          "an",
                          "pa",
                          "ch",
                          "nodes",
                          "edges",                          
                          "is.complete",
                          "simplicialNodes",
                          "is.simplicial",
                          "ancestralSet",
                          "ancestralGraph"

                          ))
  
  nelobject <- .grash2nel(object)

  switch(type,
         ## Functions from graph package here.
         ##

         "maxClique"=,"cliques"={
           maxClique(nelobject)$maxCliques
         },

         "connectedComp"=,"concomp"={
           connectedComp(nelobject)
         },

         "separates"={
           separates(set, set2, set3, nelobject)
         },

         "adj"={
           adj(nelobject, set)
         },

         "cl"={
           unique(c(set, unlist(adj(nelobject, set))))
         },

         "ne"={
           x   <-  adj(nelobject, set)
           setdiff(unique(unlist(x)),set)
         },
                  
         "is.triangulated"={
           is.triangulated(nelobject)
         },
         
         "subgraph"={
           .nel2grash(subGraph(set, nelobject))
         },         
         ## !!

         
         ## SHD graph functions here
         "an"={
           setorig <- set
           An <- set
           x <- .grash2adjmat(object)
           x <- x[-match(set, rownames(x)),]

           repeat{
             set2 <- rowSums(x[,set, drop=FALSE])
             set <- names(which(set2>0))
             if (!length(set))
               break()
             An <- c(An, set)
             x <- x[set2 == 0,,drop=FALSE]
           }

           setdiff(An, setorig)
         },

         "pa"={
           x <- .grash2adjmat(object)[,set,drop=FALSE]
           x <- rep(rownames(x), ncol(x))[which(x>0)]
           x <- setdiff(unique(x),set)
           if (length(x))x else NULL
         },
         
         "ch"={
           x <- as.adjmat(object)[set,,drop=FALSE]
           x <- rep(colnames(x), each=nrow(x))[which(x>0)]
           x <- setdiff(unique(x),set)
           if (length(x))x else NULL           
         },

         "nodes"={
           nodes(object)
         },
         "edges"={
           edges(object)
         },
         "is.complete"={
           isComplete(object)
         },
         "simplicialNodes"={
           simplicialNodes(object)
         },
         "is.simplicial"={
           isSimplicial(object, set)
         },
         "ancestralSet"={
           .ancestralSet(object, set)
         },
         "ancestralGraph"={
           .ancestralGraph(object, set)
         }
         )  
}


.ancestralGraph <- function(dag, set){
  A  <- .ancestralSet(dag, set)
  ##g<-convertg(dag, "NEL")
  g  <- .grash2nel(dag)
  sg <- subGraph(A,g) 
  sg <- .nel2grash(sg)
  sg
  }

.ancestralSet <- function(dag, set){
  amat <- as.adjmat(dag)
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


