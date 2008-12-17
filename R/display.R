##
## plot (gRain)
##

"plot.cpt-grain" <- function(x, ...){
  .plot.graphsh(x$dag)
}

"plot.dag-grain" <- function(x, ...){
  .plot.graphsh(x$dag)
}

"plot.ug-grain" <- function(x, ...){
  .plot.graphsh(x$ug)
}

plot.compgrain <- function(x, type="ug", ...){
  type <- match.arg(type, c("dag", "mdag", "ug"))
  zz <- x[[type]]
  if (!is.null(zz))
    .plot.graphsh(zz)
  else
    cat("Slot", type, "does not exist \n") 
}






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



