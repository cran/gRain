

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
    attr(ev, "pFinding") <- pFinding(object)
    object$finding <- ev
  }
  .timing(" Time: propagation:", object$control, t0)

  return(object)
}

## Lauritzen Spiegelhalter propagation
##

propagateLS <- function(APlist, rip, initialize=TRUE, details=0){

    ##  details=1
  .infoPrint(details, 1, cat(".Propagating BN: [propagateLS]\n"))

  ## FIXME: Don't remember the idea behind the 'initialize' argument; should always be true

  cliq       <- rip$cliques
  seps       <- rip$separators
  pa         <- rip$parent
  childList  <- rip$childList
  ncliq      <- length(cliq)

  ## This assignment is needed because RIP now returns 0 as the
  ## parent index for the first clique
  pa[pa==0]<-NA

  ## Backward propagation (collect evidence) towards root of junction tree
  ##
  .infoPrint(details,2, cat("..BACKWARD:\n"))
  t0 <- proc.time()
  if (ncliq>1){
    for (ii in ncliq:2){
      cq       <- cliq[[ii]]
      sp   <- seps[[ii]]
      .infoPrint2(details, 2, "Clique %d: {%s}\n", ii, .colstr( cq ))
      cq.pot   <- APlist[[ii]]
      pa.pot   <- APlist[[pa[ii]]]

      ##cat(sprintf("......is.na: cq.pot=%i, pa.pot=%i\n", any(is.na(cq.pot)), any(is.na(pa.pot))))

      if (length(sp)>=1 && !is.na(sp)){
        .infoPrint2(details, 2, "Marg onto sep {%s}\n", .colstr(sp))
        sp.pot           <- tableMargin(cq.pot, sp)
        ##cat(sprintf("......is.na: sp.pot=%i\n", any(is.na(sp.pot))))
        ## str(list(cliq.no=ii, cliq.pa.no=pa[ii],
        ##                  cq=cq, sp=sp,
        ##                  cq.pot=cq.pot, pa.pot=pa.pot, sp.pot=sp.pot))

        APlist[[ii]]     <- tableOp2(cq.pot, sp.pot, `/`)
        APlist[[pa[ii]]] <- tableOp2(pa.pot, sp.pot, `*`)
      } else{
        zzz              <- sum(cq.pot)
        APlist[[1]]      <- APlist[[1]] * zzz
        APlist[[ii]]     <- cq.pot / zzz
      }
    }
  }

  ## print(sapply(APlist, function(x) any(is.na(x))))
  ## cat("propagateLS\n"); print(as.data.frame.table(APlist[[1]]))
  normConst <- sum(APlist[[1]])

  APlist[[1]] <- APlist[[1]]/normConst

  ## Forward propagation (distribute evidence) away from root of junction tree
  ##
  .infoPrint(details,2,cat("..FORWARD:\n"))
  t0 <- proc.time()
  for (ii in 1:ncliq){
    .infoPrint2(details, 2, "Clique %d: {%s}\n", ii, .colstr(cliq[[ii]]))
    ##     ch <- which(pa[-1]==ii)+1
    ##     cat(sprintf("ii=%3d, ch=%s\n", ii, toString(ch)))
    ch <- childList[[ii]]
    if (length(ch)>0){
      .infoPrint2(details,2, "..Children: %s\n", .colstr(ch))
      for (jj in 1:length(ch)){
        if (length(seps[[ch[jj]]])>0){
          .infoPrint2(details, 2, "Marg onto sep %i: {%s}\n", ch[jj], .colstr(seps[[ch[jj]]]))
          sp.pot            <- tableMargin(APlist[[ii]], seps[[ch[jj]]])
          ##cat(sprintf("......is.na: sp.pot=%i\n", any(is.na(sp.pot))))
          APlist[[ch[jj]]]  <- tableOp2(APlist[[ch[jj]]], sp.pot, `*`)
          .infoPrint(details, 4, { cat("Marginal:\n"); print (sp.pot) })
        }
      }
    }
  }


  attr(APlist, "pFinding") <- normConst
  APlist
}
