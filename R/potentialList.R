## Create potential list (rip, universe)
##
.defaultPotentialList <- function(rip.order, universe){
  cli     <- rip.order$cliques
  APlist <- as.list(rep(NA,length(cli)))
  for (ii in 1:length(cli))
    {
      cc    <- cli[[ii]]
      vlab  <- universe$levels[cc]
      APlist[[ii]] <- parray(cc, vlab)
    }
  APlist
}

.defaultPotentialList <- function(rip.order, universe){
  cli     <- rip.order$cliques
  APlist <- as.list(rep(NA,length(cli)))
  for (ii in 1:length(cli))
    {
      cc    <- cli[[ii]]
      vlab  <- universe$levels[cc]
##       print(cc); print(vlab)
##       ccc <<- cc
##       vvv <<- vlab
      
      APlist[[ii]] <- parray(cc, vlab)
    }
  APlist
}




## Create potential list (cliques, gmData)
##
.createPotentialList <- function(rip, gmd){
  cli     <- rip$cliques
  APlist <- as.list(rep(NA,length(cli)))
  for (ii in 1:length(cli))
    {
      cc    <- cli[[ii]]
      vlab  <- valueLabels(gmd)[cc]
      APlist[[ii]] <- parray(cc, vlab)
    }
  APlist
}


## Insert cpt's into potential list (cptlist, APlist)
##
.insertCpt <- function(cptlist, APlist, rip, details=0){
  if (details>=1) cat(".Inserting cpt's in potential list [.insertCpt]\n")
  
  cli    <- rip$cliques
  lencli <- length(cli)
  amat   <- glist2setMAT(cli,vn=rip$nodes)
  
  for (ii in 1:length(cptlist))
    {
      cptc <- cptlist[[ii]]
      vert   <- varNames(cptc)     
      .infoPrint(details,2,cat("..Current cpt:",varNames(cptc),"\n"))
      jj <- which(rowSums(amat[,vert,drop=FALSE])==length(vert))[1] # Host clique
      
      APlist[[jj]] <- tableOp(APlist[[jj]], cptc, "*")    
    }
  
  .infoPrint(details, 4, {cat("....APlist (after insertion):\n"); print(APlist) })
  APlist
}



##j      <- .findHostClique(vert, cli, lencli)
##j      <- which(sapply(cli, function(d) subsetof(vert, d)))[1]
