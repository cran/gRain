
## Create potential list (cliques, gmData)
##
.createPotentialList <- function(rip, gmd){
  cli <- rip$cliques
  potlist <- as.list(rep(NA,length(cli)))
  for (i in 1:length(cli)){
    cc <- cli[[i]]
    vl <- valueLabels(gmd)[cc]
    potlist[[i]] <- ctab(cc,vl)
    ##cat("cc:", paste(cc),"length:",length(potlist[[i]]$values), "\n")
  }
  potlist
}

## Insert cpt's into potential list (cptlist, potlist)
##

 .findHostClique <- function(vert, cli, lencli){
   for (j in 1:lencli){
     if (subsetof(vert, cli[[j]])){
       break()
     }
   }
   j
 }


.insertCpt <- function(cptlist, potlist, rip, trace=0){
  if (trace>=1) cat(".Inserting cpt's in potential list [.insertCpt]\n")

  cli <- rip$cliques
  lencli <- length(cli)

  ## Note: perhaps create amat globally 
  amat <- cliquemat(cli=cli,vn=rip$nodes)
  
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
      cat("....After:\n");    print(ctabmult(potlist[[j]], cptc))
    }
    
    potlist[[j]] <- ctabmult(potlist[[j]], cptc)    
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









# propagateOLD <- function(bn){
#   bn$potlist <- .propagate(bn$potlistwork, bn$rip, initialize=TRUE, trace=bn$trace)
#   bn$initialized <- TRUE
#   bn$propagated  <- TRUE
  
#   if (!is.null(evidence(bn))){
#     ev <- evidence(bn)
#     attr(ev,"pevidence")<- pevidence(bn)
#     bn$evidence <- ev    
#   }
#   return(bn)
# }



#   if (x$initialized){
#     set <- setdiff(nodes(x$dag),evidence(x)$nodes)
#     mtab<-nodeMarginal(x, set=set)
#     lapply(mtab, print)
#     #lapply(lapply(mtab, as.numeric),print)
#   } else {
#     cat ("Network not initialized (use propagate()) to do so...\n")
#   }





# ## Hmmm think about this one....
# .getlevels <-function(gmData, v){
#  gmData$levels[match(v,gmData$name)]
# }



# makebn <- function(cptlist, gmData, description="ProbNet",
#                    root=NULL, propagate=FALSE, trace=0){

#   if(trace>=1) cat(".Creating DAG from cpt-list\n")
#   dag <- newdagsh(lapply(cptlist, ctabnames))
#   if (is.null(dag)){
#     cat("Graph defined by the cpt's is not acyclical...\n")
#     return(NULL)
#   }
  
#   elorder <- eliminationOrder(dag)
#   nn      <-lapply(cptlist, function(x)x$names[1])
#   names(cptlist) <- nn
#   cptlist <- cptlist[elorder]
  
#   if(trace>=1) cat(".Moralizing DAG\n")
#   mdag   <- moralize(dag)

#   if(trace>=1) cat(".Triangulating moralized DAG\n")
#   tmdag      <- triangulate(mdag,root=root)

#   if(trace>=1) cat(".Finding RIP order of cliques of triangulated moralized DAG\n")
#   rip     <- ripOrder(tmdag,root=root)
  
#   dummypotlist <- .createPotentialList(rip,gmData)

#   potlist <- potlistwork <- potlistorig <- .insertCpt(cptlist, dummypotlist, rip, trace)

#   bn      <- list(dag         = dag,
#                   mdag        = mdag,
#                   tmdag       = tmdag,
#                   rip         = rip,
#                   potlist     = potlist,
#                   potlistwork = potlistwork,
#                   potlistorig = potlistorig, 
#                   gmData      = gmData,
#                   elorder     = elorder,
#                   description = description,
#                   cptlist     = cptlist,
#                   trace       = trace,
#                   initialized = FALSE,
#                   propagated  = FALSE,
#                   )
#   class(bn)<-'bayesnet'

#   if (propagate){
#     if (trace>=1) cat (".Initializing network\n")
#     bn             <- propagate(bn)
#   }
#   return(bn)
# }


# print.bayesnet <- function(x,...) {
#   cat("Probabilistic network\n")
#   cat("Description : ", x$description, "\n")

#   cat("Status      :  ")
#   if (x$initialized){
#     cat ("Network is initialized...\n")
#   } else {
#     cat ("Network is not initialized...\n")
#   }
#    if (!is.null(x$evidence)){
#      print(evidence(x))
#    }
# }



##nodeNames.bayesnet <- function(x) nodes(x$dag)
##nodeStates.bayesnet <- function(x, nodes=nodeNames(x)){
##  vl<-valueLabels(x$gmData)
##  vl[nodes]
##}
