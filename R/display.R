##
## plot (gRain)
##

plot.grain <- function(x, type, ...){
  #cat("plot.grain; type:", type, "\n")
   if (!require("Rgraphviz")){
     cat("The Rgraphviz package (from Bioconductor) must be installed to display the models\n")
     return()
   }

  if (missing(type)){
    if (x$isCompiled){
      plot(x$ug)
    } else {
      if ("pot-grain" %in% class(x)){
        plot(x$ug)
      } else {
        plot(x$dag)
      }
    }
  } else {
    zz <- x[[type]]
    if (!is.null(zz))
      plot(zz)
    else
      cat("Slot", type, "does not exist \n") 
  }
}

iplot.grain <- function(x,type, ...){
  #.primiplot(x$dag)

  if (missing(type)){
    if (x$isCompiled){
      .primiplot(x$ug)
    } else {
      if ("pot-grain" %in% class(x)){
        .primiplot(x$ug)
      } else {
        .primiplot(x$dag)
      }
    }
  } else {
    zz <- x[[type]]
    if (!is.null(zz))
      .primiplot(zz)
    else
      cat("Slot", type, "does not exist \n") 
  }
}

.primiplot <- function(grp){
  ig <- igraph.from.graphNEL(grp)
  V(ig)$label <- V(ig)$name
  V(ig)$size  <- 40
  ig$cex   <-  4
  ig$layout   <- layout.graphopt
  plot(ig)
}



########################################################################


## "plot.cpt-grain" <- function(x, ...){
##   plot(x$dag)
## }

## "plot.dag-grain" <- function(x, ...){
##   plot(x$dag)
## }

## "plot.ug-grain" <- function(x, ...){
##   plot(x$ug)
## }

## plot.compgrain <- function(x, type="ug", ...){
##   type <- match.arg(type, c("dag", "mdag", "ug"))
##   zz <- x[[type]]
##   if (!is.null(zz))
##     .plot.graphsh(zz)
##   else
##     cat("Slot", type, "does not exist \n") 
## }



