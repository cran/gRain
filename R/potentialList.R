## Create potential list (rip, universe)
##
.createPotList <- function(rip.order, universe){
  cli     <- rip.order$cliques
  APlist  <- as.list(rep(NA,length(cli)))
  for (ii in 1:length(cli)) {
    cc    <- cli[[ii]]
    vlab  <- universe$levels[cc]
    APlist[[ii]] <- parray(cc, vlab)
  }
  APlist
}

## Insert cpt's into potential list (cptlist, APlist)
##
.insertCpt <- function(cptlist, APlist, details=0){
  if (details>=1) cat(".Inserting cpt's in potential list [.insertCpt]\n")

  APnames <- lapply(APlist, function(x) names(dimnames(x)))
  CPnames <- unname(lapply(cptlist, function(x) varNames(x)))
  hosts    <- .findHosts( CPnames, APnames )

  for (ii in 1:length(cptlist)) {
    cptc <- cptlist[[ ii ]]
    jj   <- hosts[ ii ]
    APlist[[jj]] <- tableOp( APlist[[jj]], cptc, "*" )    
  }  
  .infoPrint(details, 4, {cat("....APlist (after insertion):\n"); print(APlist) })
  APlist
}

.insertNA <- function(list.of.tables){
  lapply(list.of.tables,
         function(xxx){ xxx[] <- NA; xxx})
}


.findHosts <- function( xx, yy ){
  unlist(lapply(1:length(xx), function(ii) which(isin(yy, xx[[ ii ]], index=T)>0)[1]))
}


















  ##      cli    <- rip$cliques
  ##      amat   <- glist2setMAT(cli,vn=rip$nodes)

    ##     vert   <- varNames(cptc)     
    ##     .infoPrint(details,2,cat("..Current cpt:",varNames(cptc),"\n"))
    #jj <- which(rowSums(amat[,vert,drop=FALSE])==length(vert))[1] # Host clique



## Create potential list (cliques, gmData)
##
## .createPotentialList <- function(rip, gmd){
##   cli     <- rip$cliques
##   APlist <- as.list(rep(NA,length(cli)))
##   for (ii in 1:length(cli)) {
##     cc    <- cli[[ii]]
##     vlab  <- valueLabels(gmd)[cc]
##     APlist[[ii]] <- parray(cc, vlab)
##   }
##   APlist
## }
