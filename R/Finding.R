
pFinding <- function(object)
  attr(object$potlist,"pFinding")


getFinding <- function(object){
  object$finding
}


print.grainFinding <- function(x, ...){
    cat("Finding: \n") 
    v<-do.call("cbind",x) 
    colnames(v) <- c("variable", "state")
    print(v, quote=FALSE)
    if (!is.null(attr(x,"pFinding")))
      cat("Pr(Finding)=",attr(x,"pFinding"),"\n")
    return(x)
    }


setFinding <- function(object, nodes=NULL, states=NULL, flist=NULL, propagate=TRUE){

  if (!object$compiled){
    ##cat("setFinding: Compiling model ...\n")
    object <- compile(object)
  }
    
  if (!is.null(flist)){
    flist2 <- do.call("rbind",flist)
    nodes   <- flist2[,1]
    states  <- flist2[,2]
  }

  bnnodes   <- object$nodes 
  currev    <- getFinding(object)
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
    
    object$potlistwork  <- .insertFinding(nodes, states, object$potlistwork, object$rip)
    object$initialized  <- FALSE

    if (!is.null(currev)){
      ev <- list(nodes=c(currev$nodes,nodes), states=c(currev$states,states))
    } else {
      ev <- list(nodes=nodes, states=states)
    }

    class(ev)<-"grainFinding"
    object$finding <- ev

    if (object$control$timing)
      cat("Time: enter finding", proc.time()-t0, "\n")

    if (propagate){
      object<-propagate(object)
    } else {
      object$propagated <- FALSE
    }
  } 

  return(object)
}

.insertFinding <- function(nodes, states, potlist, rip, trace=0){

  if (trace>=1) cat(".function: .insertCpt\n")
  cli <- rip$cliques

  ## Note: perhaps create amat globally 
  amat <- as.setmat(cli,vn=rip$nodes)


  
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

##       lev    <- valueLabels.array(cpot)[[currn]] ## BRIS
##       print(lev)

      lev <- dimnames(cpot)[[currn]]
#            print(lev)
      evTab  <- evidenceTable(currn, currs, lev)
      potlist[[j]]  <- tableOp(cpot, evTab, "*")
    }
  }
  potlist
}

evidenceTable <- function(node, state, levels){
  pot   <- rep.int(0,length(levels))
  pot[match(state, levels)] <- 1
  t2  <- ptable(node, list(levels), pot)
  t2
}


retractFinding <- function(object, nodes=NULL, propagate=TRUE){
  if (is.null(nodes))
    return(.resetgrain(object))

  ev <- getFinding(object)
  evnodes   <- ev$nodes
  evstates <- ev$states
  
  idx<-match(intersect(evnodes,nodes), evnodes)
  if (length(idx)>0){
    newevnodes  <- evnodes[-idx]
    newevstates <- evstates[-idx]
    object <- .resetgrain(object)
    object <- setFinding(object, nodes=newevnodes, states=newevstates, propagate=propagate)
  }
  return(object)
}      





