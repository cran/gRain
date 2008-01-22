##
## Graph operations - simple
##

is.completesetMAT <- function(set, amat){
  if (!length(set))
    return(TRUE)
  
  subm <- amat[set,set]
  all(subm[upper.tri(subm)])
}


is.completeset <- function(set,ed){
##---------------------------------
## set: variables
## ed : edges = list of pairs
  if (!length(set))
    return(TRUE)
  ve  <- names2pairs(set)
  led <- length(ed)
  lve <- length(ve)
  is.comp <- TRUE
  i <- 1
  repeat{
    ce <- ve[[i]]
    for (j in 1:led) {
      if (subsetof(ce, ed[[j]])){
        break()
      }
      if (j==led){
        is.comp<-FALSE
      }
    }
    if (!is.comp || i ==lve){
      break()
    }
    i <- i + 1
  }  
  return(is.comp)
}











#############

simplicialNodes <- function(ug){
  nodes <- nodes(ug)
  b     <- sapply(nodes, function(s) isSimplicial(ug,s))
  sim   <- nodes[b]
  return(sim)
}

neigh <- function(ug,v){  
  e     <- edges(ug)
  idxb  <- sapply(e, function(d) v %in% d)
  unique(setdiff(unlist(e[idxb]),v))
}

closure <- function(ug, v){
  c(v,neigh(ug,v))
}

subgraph <- function(g, v){
  if (!length(v)){
    newugsh(NULL)
  } else {
    ed <- edges(g)
    if (length(ed)){
      x<-sapply(ed, subsetof, v)
      newugsh(c(ed[which(x)],v))
    } else {
      newugsh(v)
    }
  }
}


isSimplicial <- function(ug, v){
  ##isComplete(subgraph(ug,c(v, neigh(ug,v))))
  s <- queryg(ug, "ne", v)
  isComplete(queryg(ug, "subgraph", s))
}



isComplete <- function(ug){

  v <- nodes(ug)
  if(length(v)==0)
    return(TRUE)
  e <- edges(ug)
  uniqueEdges <- unique(lapply(e, sort))
  
  length(v)*(length(v)-1)/2 == length(uniqueEdges)

}

