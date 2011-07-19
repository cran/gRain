### #####################################################
###
### Creating grain objects
###
### #####################################################
grain <- function(x, data=NULL, control=list(), smooth=0, details=0,...){
  UseMethod("grain")
}


## A list of clique potentials (defined over a chordal graph)
## 
## NOTICE: POTspec contains terms p(R_k|S_k) as used in 
##    p(V)=\prod_k p(R_k|S_k)
## i.e. the 'set chain representation'.

grain.POTspec <- function(x, data=NULL, control=list(), smooth=0, details=0,...){
  ## cat("grain.POTspec\n")
  
  control  <- .setControl(control)
  universe <- attributes(x)[c("nodes","levels","nlev")]
  ans  <- c(list(universe    = universe,
                 data        = data,
                 equilCQpot= c(x),  
                 nodes       = unname(nodes(attr(x, "ug"))),
                 ug          = attr(x, "ug"),
                 dag         = attr(x, "dag"),    ## FIXME: We carry this info...
                 cptlist     = attr(x, "cptlist"),
                 rip         = attr(x, "rip")
                 ),
            .setExtraComponents(control, details))
  class(ans) <- c("pot-grain","grain")    
  ans
}

## A list of cpt's
##
grain.CPTspec <- function(x, data=NULL, control=list(), smooth=0, details=0,...)
{
  ##cat("grain.CPTspec\n")
  control  <- .setControl(control)
  att      <- attributes(x)
  dag      <- att$dag
  universe <- att[c("nodes","levels","nlev")]
  ans  <- c(list(universe    = universe,
                 data        = data,
                 nodes       = unname(nodes(dag)),
                 dag         = dag,
                 cptlist     = c( x )),
            .setExtraComponents(control, details))

  class(ans) <- c("cpt-grain","grain")
  return(ans)
}

## A graph + data (wrappers for calling grain.POTspec and grain.CPTspec)
##
grain.graphNEL <- function(x, data=NULL, control=list(), smooth=0, details=0,...){

  if (missing(data))
    stop("Data must be given to create grain from graph\n")
  if (!(is.array(data) || is.data.frame(data)))
    stop("Data must be an array or a dataframe\n")
  
  switch(edgemode(x), 
         "directed"={
           ans <- grain(compileCPT(extractCPT(data, x, smooth=smooth)),
                        data=data, control=control, details=details)           
         },
         "undirected" = {
           ans <- grain(compilePOT(extractPOT(data, x, smooth=smooth)),
                        data=data, control=control, details=details)
         })
  return(ans)
}

## Printing grain
##
print.grain <- function(x,...){
  cat("Independence network: Compiled:", x$isCompiled, "Propagated:", x$isPropagated, "\n")
  cat(" Nodes:")
  str(unname(nodeNames(x)))
  if (length(x$finding)>0){
    cat(" Findings:")
    str(x$finding$nodes)
  }
  return(invisible(x))
}

.setControl <- function(control){
  con <- list(timing=0)
  con[(namc <- names(control))] <- control
  con
}

.setExtraComponents <- function(control, details){
  list(
       isInitialized = FALSE,
       isCompiled    = FALSE,
       isPropagated  = FALSE,
       finding       = NULL, 
       control       = .setControl(control),
       details       = details
     )
}

