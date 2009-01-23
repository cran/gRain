
propagate.compgrain <- function(object, trace=object$trace, ...){

  t0 <- proc.time()
  object$potlist <- .propagate(object$potlistwork, object$rip,
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

.propagate <- function(potlist, rip, initialize=FALSE, trace=0){
  if (trace>=1) cat(".Propagating BN: [.propagate]\n")

  cliq   <- rip$cliques
  seps   <- rip$separators
  pa     <- rip$parent
  ncliq  <- length(cliq)

  ## FIXME: This is a hack introduced because RIP now returns 0 as the
  ## parent index for the first clique
  pa[pa==0]<-NA
    
  if(trace>=2) cat("..BACKWARD:\n")
  t0 <- proc.time()
  if (ncliq>1){
    for (i in ncliq:2){      
      if(trace>=2) cat("..Current clique:",i,"             {",cliq[[i]],"}", "\n")      
      cpot  <- potlist[[i]];      
      csep  <- seps[[i]]
      cpa   <- potlist[[pa[i]]]

      if (length(csep)>=1 && !is.na(csep)){
        if(trace>=2) cat("..Marginalize onto separator :", "  {", csep,"}", "\n")
        
        ##septab   <- tableMarginPrim(cpot, csep)
        septab   <- tableMargin(cpot, csep)
        ##cpotnew  <- tableOp(cpot, septab, "/")
        cpotnew  <- .tableOp2(cpot, septab, `/`)             
        potlist[[i]]     <- cpotnew        
        ##potlist[[pa[i]]] <- tableOp(cpa, septab, "*")
        potlist[[pa[i]]] <- .tableOp2(cpa, septab, `*`) 

        if(trace>=4) {
          cat("....Dividing by marginal\n"); print (septab); print (cpot); print(cpotnew)
          cat("....Parent potential\n"); print(cpa);
        }

      } else{        
        potlist[[1]] <- potlist[[1]] * sum(cpot) 
        cpot         <- cpot / sum(cpot) 
        potlist[[i]] <- cpot
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

  if(trace>=2)cat("..FORWARD:\n")

  t0 <- proc.time()
  for (i in 1:ncliq){
    
    if(trace>=2) cat("..Current clique:",i,"             {",cliq[[i]],"}", "\n")      
    ch <- which(pa[-1]==i)+1

    if (length(ch)>0){
      if (trace>=2) cat("..Children:", ch, "\n")
      for (j in 1:length(ch)){

        if (length(seps[[ch[j]]])>0){
          if(trace>=2)
            cat("..Marginalize onto separator", ch[j], ": {", seps[[ch[j]]]," }\n")

          septab <- tableMarginPrim(potlist[[i]], seps[[ch[j]]])

          if(trace>=4)
            {cat("Marginal:\n"); print (septab)}

          ##potlist[[ch[j]]] <- tableOp(potlist[[ch[j]]], septab, "*") ##newpot
          potlist[[ch[j]]] <- .tableOp2(potlist[[ch[j]]], septab, `*`) ##newpot
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
