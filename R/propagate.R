
propagate.grain <- function(object, trace=object$trace, ...){

  t0 <- proc.time()
  object$potlist <- propagateLS(object$potlistwork, object$rip,
                               initialize=TRUE, trace=trace)

  object$initialized <- TRUE
  object$propagated  <- TRUE
  
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
propagateLS <- function(potlist, rip, initialize=TRUE, trace=0){
  if (trace>=1) cat(".Propagating BN: [propagateLS]\n")

  cliq   <- rip$cliques
  seps   <- rip$separators
  pa     <- rip$parent
  ncliq  <- length(cliq)

  ## FIXME: This is a hack introduced because RIP now returns 0 as the
  ## parent index for the first clique
  pa[pa==0]<-NA

  ## Backward propagation (collect evidence) towards root of junction tree
  ##
  if(trace>=2) cat("..BACKWARD:\n")
  t0 <- proc.time()
  if (ncliq>1){
    for (ii in ncliq:2){      
      if(trace>=2) cat("..Current clique:",ii,"             {",cliq[[ii]],"}", "\n")      
      cpot  <- potlist[[ii]];      
      csep  <- seps[[ii]]
      cpa   <- potlist[[pa[ii]]]

      if (length(csep)>=1 && !is.na(csep)){
        if(trace>=2) cat("..Marginalize onto separator :", "  {", csep,"}", "\n")
        
        septab            <- tableMargin(cpot, csep)
        potlist[[ii]]     <- tableOp2(cpot, septab, `/`)             
        potlist[[pa[ii]]] <- tableOp2(cpa,  septab, `*`) 

        if(trace>=4) {
          cat("....Dividing by marginal\n"); print (septab); print (cpot); 
          cat("....Parent potential\n"); print(cpa);
        }

      } else{
        zzz <- sum(cpot)
        potlist[[1]]  <- potlist[[1]] * zzz
        potlist[[ii]] <- cpot / zzz
      }
    }
  }

  normConst <- sum(potlist[[1]]) 

  if (normConst==0){
    stop("Propagation of inconsistent finding has been attempted...\n",call.=FALSE)
  }

  if (initialize){
    potlist[[1]] <- potlist[[1]]/normConst 
  }
  
  if (trace>=4) {
    cat("....BACKWARD done - potlist - After backward propagation:\n"); print(potlist)
    cat("....Normalizing constant:\n");  print(normConst)
  }

  ## Forward propagation (distribute evidence) away from root of junction tree
  ##
  
  if(trace>=2)cat("..FORWARD:\n")
  t0 <- proc.time()
  for (ii in 1:ncliq){
    if(trace>=2) cat("..Current clique:",ii,"             {",cliq[[ii]],"}", "\n")      
    ch <- which(pa[-1]==ii)+1

    if (length(ch)>0){
      if (trace>=2)
        cat("..Children:", ch, "\n")
      for (jj in 1:length(ch)){
        if (length(seps[[ch[jj]]])>0){
          if(trace>=2)
            { cat("..Marginalize onto separator", ch[jj], ": {", seps[[ch[jj]]]," }\n") }
          
          septab            <- tableMargin(potlist[[ii]], seps[[ch[jj]]])
          potlist[[ch[jj]]] <- tableOp2(potlist[[ch[jj]]], septab, `*`) 

          if(trace>=4)
            { cat("Marginal:\n"); print (septab) }
          
        }
      }
    }
  }

  attr(potlist, "pFinding") <- normConst
  if (trace>=4) {
    cat("....FORWARD done - potlist - After forward propagation:\n");    print(potlist)
  }
  potlist
}
