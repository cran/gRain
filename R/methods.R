
nodeNames  <- function(x) UseMethod("nodeNames")

nodeStates <- function(x, nodes=nodeNames(x)) UseMethod("nodeStates")

## nodeNames.grain  <- function(x) x$nodes
## nodeStates.grain <- function(x, nodes=nodeNames(x)){
##   vl<-valueLabels(x$gmData)
##   vl[nodes]
## }

nodeNames.grain  <- function(x)
  x$universe$nodes

nodeStates.grain <- function(x, nodes=nodeNames(x)){
  x$universe$levels[nodes]
}
