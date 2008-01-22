##
## display (gRbayesnet)
##

"plot.cpt-gmInstance" <- function(x, ...){
  .plot.graphsh(x$dag)
}

"plot.dag-gmInstance" <- function(x, ...){
  .plot.graphsh(x$dag)
}

"plot.ug-gmInstance" <- function(x, ...){
  .plot.graphsh(x$ug)
}

plot.compgmInstance <- function(x, ...){
  .plot.graphsh(x$rip$tug)
}

plot.ugsh <- function(x, ...){
  .plot.graphsh(x)
}

plot.dagsh <- function(x, ...){
  .plot.graphsh(x)
}

.plot.graphsh <- function(graph){

  if (length(graph)==0)
    return(NULL)


  edges <- edges(graph)
  nodes <- nodes(graph)

  nAttrs <- list()
  nodeColours        <- rep("yellow", length(nodes))
  names(nodeColours) <- nodes
  nAttrs$fillcolor   <- nodeColours
  
  edgeColours        <- rep("blue",length(edges))
  names(edgeColours) <- sapply(edges, function(ee) {
    estr <- paste(ee[1],"~",ee[2],sep='');
    estr
  })

  eAttrs             <- list(color=edgeColours)


  if (class(graph)[1]=="ugsh"){
    G <- new("graphNEL", nodes=nodes,edgemode='undirected')
  } else {
    G <- new("graphNEL", nodes=nodes,edgemode='directed')
  }

  if (length(edges)>0){
    for (i in 1:length(edges)){
      ee <- rev(edges[[i]]);  ##  cat("Adding edge:", paste(ee),"\n")
      ##G <- addEdge(ee[1], ee[2] , G, weight=1)
      G <- addEdge(ee[1], ee[2], G)
    }
  }
  plot(G, "neato", nodeAttrs = nAttrs, edgeAttrs = eAttrs)

  return(invisible(graph))
}

