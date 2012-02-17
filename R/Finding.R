
pFinding <- function(object)
  attr(object$equilCQpot,"pFinding")


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

  ##  cat("setFinding\n")
  if (!object$isCompiled){
    ##cat("setFinding: Compiling model ...\n")
    object <- compile(object)
  }
    
  if (!is.null(flist)){
    flist2 <- do.call("rbind",flist)
    nodes   <- flist2[,1]
    states  <- flist2[,2]
  }

  len            <- length(nodes)

  if (len>0){
  
    netNodes       <- object$nodes 
    currFinding    <- getFinding(object)
    
    
    for (i in 1:len){
      ev1   <- nodes[i]; 
      if (!(ev1 %in% netNodes)){
        nodes[i] <- states[i] <- NA
        ##warning("Node ", ev1, " is not in network, skipping it...\n",call.=FALSE)
      }
    }
    
    ## Ignore NA's in the states
    ##
    nodes  <- nodes[!is.na(states)]
    states <- states[!is.na(states)]
    
    ## Drop nodes which are already given evidence in the network
    ##
    if (!is.null(currFinding)){
      idx <- match(intersect(currFinding$nodes, nodes), nodes)
      if (length(idx)>0){
        nodes  <- nodes[-idx]
        states <- states[-idx]
      }
    }
    
    ##  print(nodes)
    ##  print(states)
    ##  Now insert the findings
    ##
    if (length(nodes)>0){
      t0 <- proc.time()    
      ## setFinding: findings are insertet to tempCQpot
      object$tempCQpot      <- .insertFinding(nodes, states, object$tempCQpot, object$rip)   
      object$isInitialized  <- FALSE
      
      if (!is.null(currFinding)){
        ev <- list(nodes=c(currFinding$nodes,nodes), states=c(currFinding$states,states))
      } else {
        ev <- list(nodes=nodes, states=states)
      }
      
      ## Set finding slot
      class(ev)<-"grainFinding"
      object$finding <- ev
      
      if (object$control$timing)
        cat("Time: enter finding", proc.time()-t0, "\n")
      
      if (propagate){
        object<-propagate(object)
      } else {
        object$isPropagated <- FALSE
      }
    }
  }
  return(object)
}



.insertFinding <- function(nodes, states, APlist, rip, details=0){

  .infoPrint(details, 1, cat(".insertFinding\n"))
  cli <- rip$cliques

  ## Note: perhaps create amat globally 
  amat <- .as.setmat(cli,vn=rip$nodes)
  
  for (i in 1:length(nodes)){
    currn <- nodes[i]
    currs <- states[i]
    ##cat("Node:", currn, "State:", currs, "\n")
    #idx  <- which(sapply(cli, function(x) subsetof(currn, x)))
    ##idx <- which(rowSums(amat[,currn,drop=FALSE])==1)#[1]
    idx <- which(amat[,currn]==1)#[1]

    ##cat("Host cliques:",paste(idx,sep=' '),"\n");
    for (j in idx){            
      cpot <- APlist[[j]]
      ## cat("Current clique:", paste(varNames(cpot), sep=' '),"\n")
      ## lev    <- valueLabels.array(cpot)[[currn]] ## BRIS
      lev <- dimnames(cpot)[[currn]]
      evTab  <- .evidenceTable(currn, currs, lev)
      APlist[[j]]  <- tableOp(cpot, evTab, "*")
    }
  }
  APlist
}

.evidenceTable <- function(node, state, levels){
  pot   <- rep.int(0,length(levels))
  pot[match(state, levels)] <- 1
  t2  <- parray(node, list(levels), pot)
  t2
}


retractFinding <- function(object, nodes=NULL, propagate=TRUE){

  .resetgrain <- function(xxx){
    ## retractFinding: equilCQpot is reset to origCQpot
    xxx$equilCQpot       <- xxx$origCQpot
    xxx$finding       <- NULL
    xxx$isInitialized <- TRUE
    xxx
  }

  if (is.null(nodes))
    return(.resetgrain(object))

  ev <- getFinding(object)
  evnodes   <- ev$nodes
  evstates  <- ev$states

  idx<-match(intersect(evnodes,nodes), evnodes)

  if (length(idx)>0){
    newevnodes  <- evnodes[-idx]
    newevstates <- evstates[-idx]
    
    object <- .resetgrain(object)

    if (length(newevnodes)>0){
      object <- setFinding(object, nodes=newevnodes, states=newevstates, propagate=FALSE)
    }
  }

  if (propagate){
    object<-propagate(object)
  } else {
    object$isPropagated <- FALSE
  }
  return(object)
}      





