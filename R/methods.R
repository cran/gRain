
nodeNames.grain  <- function(x) x$nodes
nodeStates.grain <- function(x, nodes=nodeNames(x)){
  vl<-valueLabels(x$gmData)
  vl[nodes]
}
