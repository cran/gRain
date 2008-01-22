summary.gmInstance <- function(object, type='std', ...){
  type <- match.arg(type,c("std","cliques","rip","configurations"))

  cat("Nodes :", object$nodes,"\n")
  isCompiled <- inherits(object, "compgmInstance")
  isPropagated <- object$propagated
  if (isCompiled){
    cat("Status: Compiled\n")
  } else {
    cat("Status: Uncompiled\n")
  }

  
  
  if (isCompiled){
    cat("Model is propagated:", isPropagated, "\n") 
    rip <- object$rip
    cl  <- rip$clique
    se  <- rip$separators
    pa  <- rip$pa
    
    cat("\n")
    cl2 <- sapply(object$rip$cliques,length)
    cat("Number of cliques:",
        length(cl2),"\n")
    cat("Maximal clique size:",
        max(cl2),"\n")
    cat("Maximal number of configurations in cliques:",
        max(unlist(  sapply(object$potlist, "[", "ncells"))), "\n")
    
    if(length(e<-evidence(object))){
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
    
    
    return(invisible(object))
  }
}

