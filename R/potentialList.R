## Create potential list (rip, universe)
##
.defaultPotentialList <- function(rip.order, universe){
  cli     <- rip.order$cliques
  potlist <- as.list(rep(NA,length(cli)))
  for (ii in 1:length(cli))
    {
      cc    <- cli[[ii]]
      vlab  <- universe$levels[cc]
      potlist[[ii]] <- ptable(cc, vlab)
    }
  potlist
}

## Create potential list (cliques, gmData)
##
.createPotentialList <- function(rip, gmd){
  cli     <- rip$cliques
  potlist <- as.list(rep(NA,length(cli)))
  for (ii in 1:length(cli))
    {
      cc    <- cli[[ii]]
      vlab  <- valueLabels(gmd)[cc]
      potlist[[ii]] <- ptable(cc, vlab)
    }
  potlist
}


## Insert cpt's into potential list (cptlist, potlist)
##
.insertCpt <- function(cptlist, potlist, rip, details=0){
  if (details>=1) cat(".Inserting cpt's in potential list [.insertCpt]\n")
  
  cli    <- rip$cliques
  lencli <- length(cli)
  amat   <- .as.setmat(cli,vn=rip$nodes)
  
  for (ii in 1:length(cptlist))
    {
      cptc <- cptlist[[ii]]
      vert   <- varNames(cptc)     
      .infoPrint(details,2,cat("..Current cpt:",varNames(cptc),"\n"))
      jj <- which(rowSums(amat[,vert,drop=FALSE])==length(vert))[1] # Host clique
      
      potlist[[jj]] <- tableOp(potlist[[jj]], cptc, "*")    
    }
  
  .infoPrint(details, 4, {cat("....potlist (after insertion):\n"); print(potlist) })
  potlist
}



##j      <- .findHostClique(vert, cli, lencli)
##j      <- which(sapply(cli, function(d) subsetof(vert, d)))[1]
