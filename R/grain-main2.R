##
## Creating grain
##

grain <- function(x, data=NULL, control=list(), trace=0,...)
{
  UseMethod("grain")
}

## A list of clique potentials (defined over a chordal graph)
## 
grain.potspec <- function(x, data=NULL, control=list(), trace=0,...){

  ##cat("grain.potspec\n")
  control <- .setControl(control)
  
  att <- attributes(x)
  ug  <- att$ug
  universe <- att[c("nodes","levels","nlev")]
  
  ans  <- c(list(universe    = universe,
                 data        = data,
                 potlist     = c(x),
                 nodes       = nodes(ug),
                 ug          = ug),
            .setExtraComponents(control, trace))
  
  class(ans) <- c("pot-grain","grain")    
  ans
}

## A list of cpt's
##
grain.cptspec <- function(x, data=NULL, control=list(), trace=0,...)
{
  ## cat("grain.cptspec\n")
  control <- .setControl(control)

  att  <- attributes(x)
  dag  <- att$dag
  universe <- att[c("nodes","levels","nlev")]
  
  ## Collect results
  ##
  ans  <- c(list(universe    = universe,
                 data        = data,
                 nodes       = nodes(dag),
                 dag         = dag,
                 cptlist     = c( x )),
            .setExtraComponents(control, trace))

  class(ans) <- c("cpt-grain","grain")
  return(ans)
}

## Just a wrapper for calling
## grain.cptspec and grain.potspec
##

grain.graphNEL <- function(x, data=NULL, control=list(), trace=0,...){

  if (missing(data))
    stop("Data must be given to create grain from graph\n")

  if (!is.array(data))
    stop("Data must be an array\n")
  
  switch(edgemode(x), 
         "directed"={
           ans <- grain(compileCPT(extractCPT(data, x)), data=data, control=control, trace=trace)           
         },
         "undirected" = {
           ans <- grain(compilePOT(extractPOT(data, x)), data=data, control=control, trace=trace )
         }
         )
  return(ans)
}


## Printing grain
##
print.grain <- function(x,...){
  cat("Independence network (2): ") #", x$description, " ")

  ##isCompiled <- inherits(x, "compgrain")
  isCompiled <- x$compiled

  isPropagated <- x$propagated

  if (is.null(isPropagated))
    isPropagated <- FALSE

  cat("Compiled:", isCompiled, "Propagated:",
      isPropagated, "\n")

  ## print(class(x))
  return(invisible(x))
}


.setControl <- function(control){
  con <- list(timing=0)
  con[(namc <- names(control))] <- control
  con
}

.setExtraComponents <- function(control, trace){
  list(
       initialized = FALSE,
       compiled    = FALSE,
       propagated  = FALSE,
       control     = .setControl(control),
       trace       = trace
     )
}

