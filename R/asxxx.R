
setMethod("edges", signature(object = "compgmInstance"),
          function(object, which) {
            edges(object$tug)
          })
          
setMethod("nodes", signature(object = "compgmInstance"),
          function(object, which) {
            nodes(object$tug)
          })
          
setMethod("edges", signature(object = "cpt-gmInstance"),
          function(object, which) {
            edges(object$dag)
          })
          
setMethod("nodes", signature(object = "cpt-gmInstance"),
          function(object, which) {
            nodes(object$dag)
          })


as.gmInstance <- function(x,control=list(),description="gmInstance", trace=0,...)
  UseMethod("as.gmInstance")

as.gmInstance.huginNet <- function(x,control=list(),description="gmInstance", trace=0,...)
{
  newgmInstance(x$cptlist, x$gmData, description=description,
                control=control, trace=trace)
}


as.probnet <- function(x,
                       gmData,
                       description = "ProbNet",
                       control=list(),
                       trace = 0,
                       ...){
  if (inherits(x, "huginNet")){
    newgmInstance(x$cptlist, x$gmData,
                  description=description, control=control, trace=trace )
  } else {
    newgmInstance(x, gmData, description=description, control=control, trace=trace)
  }
}













  ##UseMethod("as.probnet")



# as.probnet.cptspec <- function(x,
#                                gmData,
#                                description = "ProbNet",
#                                trace = 0,
#                                ...){
#   make.gmInstance(x, gmData, description=description, trace=trace)
# }

# as.probnet.huginNet <- function(x,
#                                 gmData,
#                                 description = "ProbNet",
#                                 trace = 0,
#                                 ...){
#   as.probnet(x$cptlist, x$gmData, description=description, trace=trace )
# }


# as.probnet.dagsh <- function(x,
#                              gmData,
#                              description = "ProbNet",
#                              trace = 0,
#                              ...){
#   make.gmInstance(x, gmData, description=description, trace=trace )
# }









