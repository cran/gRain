
##
## Creating grain
##

grain <- function(x, data, description="grain",  control=list(), trace=0,...)
{
  UseMethod("grain")
}

grain.list <- function(x, data, description="grain", control=list(), trace=0,...){

  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con

  cliq<-lapply(x, function(a) names(dimnames(a)))

  ug <- ugList(cliq)
  ans  <- list(
               potlist     = x,
               nodes       = nodes(ug),
               description = description,
               ug          = ug,
               
               initialized = FALSE,
               compiled    = FALSE,
               propagated  = FALSE,
               
               control     = control,
               trace       = trace)
  class(ans) <- c("list-grain","grain")    
  ans
  
  
}

grain.cptspec <- function(x, data, description="grain", control=list(), trace=0,...)
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
  ##if (missing(gmData))

  gmData <- as.gmData(x) ## FIXME: Remove gmData stuff

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

               initialized = FALSE,
               compiled    = FALSE,
               propagated  = FALSE,

               control     = control,
               trace       = trace)
  class(ans) <- c("cpt-grain","grain")

  ## Done
  ##
  return(ans)
}

grain.graphNEL <- function(x, data, description="grain", control=list(), trace=0,...){

  con <- .defaultControl()
  con[(namc <- names(control))] <- control
  control <- con

  if (!missing(data)){
    if (!("gmData" %in% class(data))){
      gmData <- as.gmData(data)
    } else {
      gmData <- data
    }
  }

  
  if (edgemode(x)=="directed"){
    ans  <- list(
                 gmData      = gmData,
                 nodes       = nodes(x),
                 description = description,
                 dag         = x,
                 
                 initialized = FALSE,
                 compiled    = FALSE,
                 propagated  = FALSE,

                 control     = control,
                 trace       = trace)
    class(ans) <- c("dag-grain","grain")
  } else {
    ans  <- list(
                 gmData      = gmData,
                 nodes       = nodes(x),
                 description = description,
                 ug          = x,


                 initialized = FALSE,
                 compiled    = FALSE,
                 propagated  = FALSE,
                 
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

  ##isCompiled <- inherits(x, "compgrain")
  isCompiled <- x$compiled

  isPropagated <- x$propagated

  if (is.null(isPropagated))
    isPropagated <- FALSE

  cat("Compiled:", isCompiled, "Propagated:",
      isPropagated, "\n")

  print(class(x))
  return(invisible(x))
}
