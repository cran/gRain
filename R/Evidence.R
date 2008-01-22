
pevidence <- function(object) attr(object$potlist,"pevidence")


evidence <- function(object){
  object$evidence
}


print.bnevidence <- function(x, ...){
    cat("Evidence: \n") 
    v<-do.call("cbind",x) 
    colnames(v) <- c("variable", "state")
    print(v, quote=FALSE)
    if (!is.null(attr(x,"pevidence")))
      cat("Pr(Evidence)=",attr(x,"pevidence"),"\n")
    return(x)
    }


enterEvidence <- function(object, nodes=NULL, states=NULL, evlist=NULL, propagate=TRUE){

  if (inherits(object, "gmInstance") && !inherits(object, "compgmInstance"))
    object <- compilegm(object)

  
  if (!is.null(evlist)){
    evlist2 <- do.call("rbind",evlist)
    nodes   <- evlist2[,1]
    states  <- evlist2[,2]
  }


  bnnodes   <- object$nodes 
  currev    <- evidence(object)
  len       <- length(nodes)

  for (i in 1:len){
    ev1   <- nodes[i]; 
    if (!(ev1 %in% bnnodes)){
      nodes[i] <- states[i] <- NA
      warning("Node ", ev1, " is not in network, skipping it...\n",call.=FALSE)
    }
  }

  nodes  <- nodes[!is.na(states)]
  states <- states[!is.na(states)]

  ## Drop nodes which are already given evidence in the network
  ##
  if (!is.null(currev)){
    idx    <- match(intersect(currev$nodes, nodes), nodes)
    if (length(idx)>0){
      nodes  <- nodes[-idx]
      states <- states[-idx]
    }
  }

  ## print(nodes); print(states)
  
  if (length(nodes)>0){
    t0 <- proc.time()
    
    object$potlistwork  <- .insertEv(nodes, states, object$potlistwork, object$rip)
    object$initialized  <- FALSE
    
    if (!is.null(currev)){
      ev <- list(nodes=c(currev$nodes,nodes), states=c(currev$states,states))
    } else {
      ev <- list(nodes=nodes, states=states)
    }

    class(ev)<-"bnevidence"
    object$evidence <- ev

    if (object$control$timing)
      cat("Time: enter evidence", proc.time()-t0, "\n")

    if (propagate)
      object<-propagate(object)

  } 

  return(object)
}

.insertEv <- function(nodes, states, potlist, rip, trace=0){

  if (trace>=1) cat(".function: .insertCpt\n")
  cli <- rip$cliques

  ## Note: perhaps create amat globally 
  amat <- cliquemat(cli=cli,vn=rip$nodes)
  
  for (i in 1:length(nodes)){
    currn <- nodes[i]
    currs <- states[i]
    ##cat("Node:", currn, "State:", currs, "\n")
    #idx  <- which(sapply(cli, function(x) subsetof(currn, x)))
    ##idx <- which(rowSums(amat[,currn,drop=FALSE])==1)#[1]
    idx <- which(amat[,currn]==1)#[1]
    
    ##cat("Host cliques:",paste(idx,sep=' '),"\n");
    for (j in idx){            
      cpot <- potlist[[j]]
      ##cat("Current clique:", paste(varNames(cpot), sep=' '),"\n")
      lev    <- cpot$levels[[currn]]
      evTab  <- evidenceTable(currn, currs, lev)
      potlist[[j]]  <- ctabmult(cpot, evTab)
    }
  }
  potlist
}

evidenceTable <- function(node, state, levels){
  pot   <- rep.int(0,length(levels))
  pot[match(state, levels)] <- 1
  t2  <- ctab(node, list(levels), pot)
  t2
}


retractEvidence <- function(object, nodes=NULL, propagate=TRUE){
  if (is.null(nodes))
    return(resetbn(object))

  ev <- evidence(object)
  evnodes   <- ev$nodes
  evstates <- ev$states
  
  idx<-match(intersect(evnodes,nodes), evnodes)
  if (length(idx)>0){
    newevnodes  <- evnodes[-idx]
    newevstates <- evstates[-idx]
    object <- resetbn(object)
    object <- enterEvidence(object, nodes=newevnodes, states=newevstates, propagate=propagate)
  }
  return(object)
}      





