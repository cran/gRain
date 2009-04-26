##
## plot (gRain)
##

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

plot.grain <- function(x, type, ...){
  #cat("plot.grain; type:", type, "\n")
   if (!require("Rgraphviz")){
     cat("The Rgraphviz package (from Bioconductor) must be installed to display the models\n")
     return()
   }

  if (missing(type)){
    if (x$compiled){
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



## .plot.graphsh <- function(graph){

##   if (!require("Rgraphviz")){
##     cat("The Rgraphviz package (from Bioconductor) must be installed to display the models\n")
##     return()
##   }

##   ###plot(as.graphNEL(graph))
##   plot(graph)
##   return(invisible(graph))
## }

########################################################################



