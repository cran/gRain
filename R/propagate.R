
propagate.grain <- function(object, details=object$details, ...){

  t0 <- proc.time()
  ## propagate.grain: equilCQpot is updated after propagation on tempCQpot
  ## such that equilCQpot will contain the updated potentials. 
  object$equilCQpot <- propagateLS(object$tempCQpot,
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

propagateLS <- function(APlist, rip, initialize=TRUE, details=0){

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
      cpot  <- APlist[[ii]];      
      csep  <- seps[[ii]]
      cpa   <- APlist[[pa[ii]]]
      if (length(csep)>=1 && !is.na(csep)){
        .infoPrint2(details, 2, "Marg onto sep {%s}\n", .colstr(csep))        
        septab           <- tableMargin(cpot, csep)
        APlist[[ii]]     <- tableOp2(cpot, septab, `/`)             
        APlist[[pa[ii]]] <- tableOp2(cpa,  septab, `*`) 
      } else{
        zzz <- sum(cpot)
        APlist[[1]]  <- APlist[[1]] * zzz
        APlist[[ii]] <- cpot / zzz
      }
    }
  }
  
  ## cat("propagateLS\n"); print(as.data.frame.table(APlist[[1]]))
  normConst <- sum(APlist[[1]]) ## ;print(normConst)
  
  if (normConst==0){
    attr(APlist, "pFinding") <- normConst
  }

  if (initialize){
    APlist[[1]] <- APlist[[1]]/normConst 
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
          septab            <- tableMargin(APlist[[ii]], seps[[ch[jj]]])
          APlist[[ch[jj]]] <- tableOp2(APlist[[ch[jj]]], septab, `*`) 
          .infoPrint(details, 4, { cat("Marginal:\n"); print (septab) })          
        }
      }
    }
  }

  attr(APlist, "pFinding") <- normConst
  APlist
}
