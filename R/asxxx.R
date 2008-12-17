as.grain <- function(x,control=list(),description="grain", trace=0,...)
  UseMethod("as.grain")

as.grain.huginNet <- function(x,control=list(),description="grain", trace=0,...)
{
  grain(x$cptlist, x$gmData, description=description,
                control=control, trace=trace)
}


## FIXME: Do we need this one???
as.probnet <- function(x,
                       gmData,
                       description = "ProbNet",
                       control=list(),
                       trace = 0,
                       ...){
  if (inherits(x, "huginNet")){
    grain(x$cptlist, x$gmData,
                  description=description, control=control, trace=trace )
  } else {
    grain(x, gmData, description=description, control=control, trace=trace)
  }
}

















