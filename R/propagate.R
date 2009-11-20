
propagate.grain <- function(object, details=object$details, ...){

  t0 <- proc.time()
  object$potlist <- propagateLS(object$potlistwork,
                                rip=object$rip, initialize=TRUE, details=details)

  object$isInitialized <- TRUE
  object$isPropagated  <- TRUE
  
  if (!is.null(getFinding(object))){
    ev <- getFinding(object)
    attr(ev,"pFinding")<- pFinding(object)
    object$finding <- ev    
  }
  if (object$control$timing){
    cat("Time: propagation", proc.time()-t0, "\n")
  }
  return(object)
}

## Lauritzen Spiegelhalter propagation
##
propagateLS <- function(potlist, rip, initialize=TRUE, details=0){

  .infoPrint(details, 1, cat(".Propagating BN: [propagateLS]\n"))

  cliq   <- rip$cliques
  seps   <- rip$separators
  pa     <- rip$parent
  ncliq  <- length(cliq)

  ## FIXME: This is a hack introduced because RIP now returns 0 as the
  ## parent index for the first clique
  pa[pa==0]<-NA

  ## Backward propagation (collect evidence) towards root of junction tree
  ##
  .infoPrint(details,2, cat("..BACKWARD:\n"))
  t0 <- proc.time()
  if (ncliq>1){
    for (ii in ncliq:2){      
      .infoPrint2(details, 2, "Clique %d: {%s}\n", ii, .colstr(cliq[[ii]]))
      cpot  <- potlist[[ii]];      
      csep  <- seps[[ii]]
      cpa   <- potlist[[pa[ii]]]

      if (length(csep)>=1 && !is.na(csep)){
        .infoPrint2(details, 2, "Marg onto sep {%s}\n", .colstr(csep))
        
        septab            <- tableMargin(cpot, csep)
        potlist[[ii]]     <- tableOp2(cpot, septab, `/`)             
        potlist[[pa[ii]]] <- tableOp2(cpa,  septab, `*`) 

      } else{
        zzz <- sum(cpot)
        potlist[[1]]  <- potlist[[1]] * zzz
        potlist[[ii]] <- cpot / zzz
      }
    }
  }

  normConst <- sum(potlist[[1]]) 

  if (normConst==0){
    attr(potlist, "pFinding") <- normConst
  }

  if (initialize){
    potlist[[1]] <- potlist[[1]]/normConst 
  }
  
  ## Forward propagation (distribute evidence) away from root of junction tree
  ##
  
  .infoPrint(details,2,cat("..FORWARD:\n"))
  t0 <- proc.time()
  for (ii in 1:ncliq){
    .infoPrint2(details, 2, "Clique %d: {%s}\n", ii, .colstr(cliq[[ii]]))
    ch <- which(pa[-1]==ii)+1

    if (length(ch)>0){
      .infoPrint2(details,2, "..Children: %s\n", .colstr(ch))
      for (jj in 1:length(ch)){
        if (length(seps[[ch[jj]]])>0){
          .infoPrint2(details, 2, "Marg onto sep %i: {%s}\n", ch[jj], .colstr(seps[[ch[jj]]]))
          septab            <- tableMargin(potlist[[ii]], seps[[ch[jj]]])
          potlist[[ch[jj]]] <- tableOp2(potlist[[ch[jj]]], septab, `*`) 
          .infoPrint(details, 4, { cat("Marginal:\n"); print (septab) })          
        }
      }
    }
  }

  attr(potlist, "pFinding") <- normConst
  potlist
}
