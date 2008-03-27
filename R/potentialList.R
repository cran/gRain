
## Create potential list (cliques, gmData)
##
.createPotentialList <- function(rip, gmd){
  cli     <- rip$cliques
  potlist <- as.list(rep(NA,length(cli)))
  for (i in 1:length(cli)){
    cc    <- cli[[i]]
    vlab  <- valueLabels(gmd)[cc]
    potlist[[i]] <- ctab(cc, vlab)
    ##cat("cc:", paste(cc),"length:",length(potlist[[i]]$values), "\n")
  }
  potlist
}

## Insert cpt's into potential list (cptlist, potlist)
##
.insertCpt <- function(cptlist, potlist, rip, trace=0){
  if (trace>=1) cat(".Inserting cpt's in potential list [.insertCpt]\n")

  cli <- rip$cliques
  lencli <- length(cli)

  ## Note: perhaps create amat globally 
  amat <- as.setmat(cli,vn=rip$nodes)
  
  for (i in 1:length(cptlist)){
    cptc <- cptlist[[i]]
    if(trace>=2) {cat("..Current cpt:",varNames(cptc),"\n"); }
    vert   <- varNames(cptc)
    #j      <- .findHostClique(vert, cli, lencli)
    #j      <- which(sapply(cli, function(d) subsetof(vert, d)))[1]
    j <- which(rowSums(amat[,vert,drop=FALSE])==length(vert))[1]
    
    if (trace>=3){
      cat("...Insert cpt", "{", vert, "}","    into potential", j,
          "  with vertices", varNames(potlist[[j]]), "\n"); 
    }
    if (trace>=4){
      cat("....Before:\n");   print(potlist[[j]])
      cat("....After:\n");    print(ctabop(potlist[[j]], cptc, "*"))
    }
    
    potlist[[j]] <- ctabop(potlist[[j]], cptc, "*")    
  }
  if (trace>=4){cat("....potlist (after insertion):\n"); print(potlist) }
  potlist
}





resetbn <- function(bn){
  ##bn$potlist     <- .propagate(bn$potlistorig, bn$rip, bn$trace)
  bn$potlist     <- bn$potlistorig
  bn$evidence    <- NULL
  bn$initialized <- TRUE
  bn
}


# .findHostClique <- function(vert, cli, lencli){
#   for (j in 1:lencli){
#     if (subsetof(vert, cli[[j]])){
#       break()
#     }
#   }
#   j
# }
