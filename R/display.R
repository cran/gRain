##
## plot (gRain)
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

## plot.ugsh <- function(x, ...){
##   .plot.graphsh(x)
## }

## plot.dagsh <- function(x, ...){
##   .plot.graphsh(x)
## }


.plot.graphsh <- function(graph){

  if (!require("Rgraphviz")){
    cat("The Rgraphviz package (from Bioconductor) must be installed to display the models\n")
    return()
  }

  ###plot(as.graphNEL(graph))
  plot(graph)
  return(invisible(graph))
}

########################################################################



