summary.grain <- function(object, type='std', ...){

  type <- match.arg(type,c("std","cliques","rip","configurations"))

  cat("Nodes :", object$nodes,"\n")
  isCompiled   <- object$compiled
  isPropagated <- object$propagated

  if (is.null(isPropagated))
    isPropagated <- FALSE
  else
    isPropagated <- TRUE
  cat("Compiled:", isCompiled, "Propagated:", isPropagated, "\n")
  
  if (isCompiled){
    rip <- object$rip
    cl  <- rip$clique
    se  <- rip$separators
    pa  <- rip$pa
    
    #cat("\n")
    cl2 <- sapply(object$rip$cliques,length)
    cat(sprintf(" Number of cliques:              %4d \n",  length(cl2)))
    cat(sprintf(" Maximal clique size:            %4d \n",  max(cl2)))
    cat(sprintf(" Maximal state space in cliques: %4d \n",
        max(unlistPrim(lapply(object$potlist, length)))))

    ##max(unlist(  sapply(object$potlist, "[", "ncells"))), "\n")
    
    if(length(e<-getFinding(object))){
      print(e)
    }
    
    switch(type,
           "rip"={
             cat("\nRIP ordering:\n")
             print(rip)
           },
           "cliques"={
             cat("\nCliques:\n")
             .printList(object$rip$cliques)
           },
           "configurations"={
             cat("\nConfigurations:\n")
             for (i in 1:length(cl)){
               cat(" ", i, ":", object$potlist[[i]]$ncells, "\n")
             }
           })
  }
  return(invisible(object))

}

