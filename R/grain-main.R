
##
## Creating grain
##

grain <- function(x, gmData, description="grain",  control=list(), trace=0,...)
{
  UseMethod("grain")
}

grain.cptspec <- function(x, gmData, description="grain", control=list(), trace=0,...)
{

  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con

  ## Create DAG from cptlist
  ##
  t0 <- proc.time()
  if(trace>=1)
    cat(".Creating DAG from cptlist\n")

  vn  <- lapply(x, varNames) # Gives (v,pa(v))
  dag <- dagList(vn)
  if (is.null(dag)){
    cat("Graph defined by the cpt's is not acyclical...\n");
    return(NULL)
  }
  if (!is.null(control$timing) && control$timing)
    cat("Time: Create dag:", proc.time()-t0,"\n")


  ## Create gmData object from cptlist
  ## FIXME: Should *not* have any effect to supply gmData
  ##
  t0 <- proc.time()
  if (missing(gmData))
    gmData <- as.gmData(x)
  if (!is.null(control$timing) && control$timing)
    cat("Time: Create gmData:", proc.time()-t0,"\n")

  ## Collect results
  ##
  ans  <- list(
               gmData      = gmData,
               nodes       = nodes(dag),
               description = description,
               dag         = dag,
               cptlist     = x,
               control     = control,
               trace       = trace)
  class(ans) <- c("cpt-grain","grain")

  ## Done
  ##
  return(ans)
}

grain.graphNEL <- function(x, gmData, description="grain", control=list(), trace=0,...){

  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con
  
  if (edgemode(x)=="directed"){
    ans  <- list(
                 gmData      = gmData,
                 nodes       = nodes(x),
                 description = description,
                 dag         = x,
                 control     = control,
                 trace       = trace)
    class(ans) <- c("dag-grain","grain")
  } else {
    ans  <- list(
                 gmData      = gmData,
                 nodes       = nodes(x),
                 description = description,
                 ug          = x,
                 control     = control,
                 trace       = trace)
    class(ans) <- c("ug-grain","grain")    
  }
  return(ans)
}

## Printing grain
##
print.grain <- function(x,...){
  cat("Independence network: ") #", x$description, " ")

  isCompiled <- inherits(x, "compgrain")

  isPropagated <- x$propagated

  if (is.null(isPropagated))
    isPropagated <- FALSE

  cat("Compiled:", isCompiled, "Propagated:",
      isPropagated, "\n")

  return(invisible(x))
}


