
propagate <- function(object, trace=object$trace) {
  UseMethod("propagate")
}

propagate.compgmInstance <- function(object, trace=object$trace){

  t0 <- proc.time()
  object$potlist <- .propagate(object$potlistwork, object$rip,
                               initialize=TRUE, trace=trace)
  object$initialized <- TRUE
  object$propagated  <- TRUE
  
  if (!is.null(evidence(object))){
    ev <- evidence(object)
    attr(ev,"pevidence")<- pevidence(object)
    object$evidence <- ev    
  }
  if (object$control$timing){
    cat("Time: propagation", proc.time()-t0, "\n")
  }
  return(object)
}


.propagate <- function(potlist, rip, initialize=FALSE, trace=0){
  if (trace>=1) cat(".Propagating BN: [.propagate]\n")

  cli   <- rip$cliques
  seps  <- rip$separators
  pa    <- rip$pa
  len   <- length(cli)

  if(trace>=2) cat("..BACKWARD:\n")
  t0 <- proc.time()
  if (len>1){
    for (i in len:2){      
      if(trace>=2) cat("..Current clique:",i,"             {",cli[[i]],"}", "\n")      
      cpot  <- potlist[[i]];      
      csep  <- seps[[i]]
      cpa   <- potlist[[pa[i]]]

      if (length(csep)>=1 && !is.na(csep)){
        if(trace>=2) cat("..Marginalize onto separator :", "  {", csep,"}", "\n")
        
        septab   <- ctabmarg(cpot, csep)
        cpotnew  <- ctabop (cpot, septab, "/")             
        potlist[[i]]     <- cpotnew        
        potlist[[pa[i]]] <- ctabop(cpa, septab, "*") 

        if(trace>=4) {
          cat("....Dividing by marginal\n")
          print (septab); print (cpot); print(cpotnew)
          cat("....Parent potential\n"); print(cpa);
        }

      } else{        
        ##potlist[[1]]$values <- potlist[[1]]$values * sum(cpot$values)
        potlist[[1]] <- potlist[[1]] * sum(cpot)  # BRIS
        ##cpot$values  <- cpot$values / sum(cpot$values)
        cpot  <- cpot / sum(cpot) # BRIS        
        potlist[[i]] <- cpot
      }
    }
  }

  ##nc <- sum(potlist[[1]]$values)
  nc <- sum(potlist[[1]])  # BRIS

  ##print(nc)
  if (nc==0){
    stop("Propagation of inconsistent evidence has been attempted...\n",call.=FALSE)
  }

  if (initialize){
    ##potlist[[1]]$values <- potlist[[1]]$values/nc
    potlist[[1]] <- potlist[[1]]/nc ## BRIS
  }
  
  if (trace>=4) {
    cat("..BACKWARD done - potlist - After backward propagation:\n"); print(potlist)
    cat("....Normalizing constant:\n");  print(nc)
  }

  if(trace>=2)cat("..FORWARD:\n")

  t0 <- proc.time()
  for (i in 1:len){
    
    if(trace>=2) cat("..Current clique:",i,"             {",cli[[i]],"}", "\n")      
    ##if(trace>=2) cat("..Current clique:",i,cli[[i]],"\n")

    ch <- which(pa[-1]==i)+1

    if (length(ch)>0){
      if (trace>=2) cat("..Children:", ch, "\n")
      for (j in 1:length(ch)){

        if (length(seps[[ch[j]]])>0){
          if(trace>=2) cat("..Marginalize onto separator", ch[j],
               ": {", seps[[ch[j]]]," }\n")

          septab <- ctabmarg(potlist[[i]], seps[[ch[j]]])

          if(trace>=4) {cat("Marginal:\n"); print (septab)}
          potlist[[ch[j]]] <- ctabop(potlist[[ch[j]]], septab, "*") ##newpot
        }
      }
    }
  }

  attr(potlist, "pevidence") <- nc
  if (trace>=4) {
    cat("....FORWARD done - potlist - After forward propagation:\n");    print(potlist)
  }
  potlist
}
