# simulate.compgmInstance <- function(object, nsim=1, seed=NULL, ...){

#   ## cptlist needs to be available. That is not the case if model
#   ## is built directly from graph.
#   ##
#   if (!inherits(object, c("dag-gmInstance","cpt-gmInstance"))){
#     cat("Can not simulate from model...\n"); return(NULL)
#   }
  
#   if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
#     runif(1)
#   if (is.null(seed)) 
#     RNGstate <- get(".Random.seed", envir = .GlobalEnv)
#   else {
#     R.seed <- get(".Random.seed", envir = .GlobalEnv)
#     set.seed(seed)
#     RNGstate <- structure(seed, kind = as.list(RNGkind()))
#     on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
#   }
  
#   bn  <- object
#   n   <- nsim
#   cptl        <- getcpt(bn)
#   elorder     <- getSlot(bn,"elorder")
#   vpa         <- vpav(getSlot(bn,"dag"))
#   names(vpa)  <- sapply(vpa, function(x)x[1])

#   ##print(elorder)
  
#   for (nid in 1:length(elorder)){
#     ##nid <- 4
#     cnode    <- elorder[nid]
#     cstates  <- nodeStates(bn)[cnode]
#     p        <- as.data.frame(cptl[[cnode]])
#     #print(cnode)
#     #print(p)
#     pa  <- vpa[[cnode]][-1]    
    
#     ## Get started
#     ##
#     if (nid==1){
#       if (!is.null(seed)){
#         set.seed(seed)
#         seedscale <- round(rgamma(1,10)+2)
#         foo <- seed*seedscale; set.seed(foo)
#       }
#       ns <- as.numeric(rmultinom(1, size = n, prob=p$potential))
#       wdata <- p
#       wdata$potential <- ns
#     } else {
#       if (length(pa)){
#         condform <- as.formula(paste("~",paste(pa, collapse='+')))
#         pl  <- splitBy(condform, p)
#         gid <- attr(pl, "groupid")
#       }
      
#       res <- as.list(rep(NA, nrow(wdata)))
#       wdata2 <- wdata; wdata2$potential <- NULL
      
#       for (i in 1:nrow(wdata)){
#         aa<-wdata[i,pa,drop=FALSE]
#         if (ncol(aa)){
#           argh <- apply(gid,1,"==", aa)
#           if (is.null(dim(argh))){
#             idx <- which(argh)
#           } else { 
#             idx <- which(apply(argh,2,all))
#           }
          
#           p <- pl[[idx]]
#         } 
#         n <- wdata[i,"potential"]
#         pot <- p$potential

#         if (!is.null(seed)){
#           set.seed(seed)
#           seedscale <- round(rgamma(1,10)+2)
#           foo <- seed*seedscale; set.seed(foo)

#         }

#         ns <- as.numeric(rmultinom(1, size = n, prob=pot))
#         ns <- cbind(expand.grid(cstates),potential=ns)
        
#         res[[i]] <- merge(wdata2[i,,drop=FALSE],ns)
        
#       }
      
#       wdata <- do.call("rbind", res)
#       wdata <- wdata[wdata$potential>0,]
#     }
#   }

#   rownames(wdata) <- 1:nrow(wdata)
#   names(wdata)[length(names(wdata))] <- "Freq"
  
#   attr(wdata, "seed") <- RNGstate
#   wdata
# }



simulate.gmInstance <- function(object, nsim=1, seed=NULL, ...){

  ## cptlist needs to be available. That is not the case if model
  ## is built directly from graph.
  ##
  #if (!inherits(object, c("dag-gmInstance","cpt-gmInstance"))){
  #  cat("Can not simulate from model...\n"); return(NULL)
  #}

  if (!inherits(object, "compgmInstance"))
    object <- compilegm(object)


  
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  if (is.null(seed)) 
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  ug        <- object$rip$tug
  dag       <- ug2dag(ug)
  elorder   <- eliminationOrder(dag)
  potlist   <- object$potlist
  vpa       <- vpav(dag)
  potnames  <- lapply(potlist, varNames)
  
  cptlist        <- as.list(rep(NA, length(vpa)))
  names(cptlist) <- sapply(vpa, "[[", 1)
  names(vpa)     <- sapply(vpa, "[[", 1)
  
  for (j in 1:length(vpa)){
    cvpa <- vpa[[j]]
    i    <- which(sapply(potnames, function(x) subsetof(cvpa, x)))[1]
    cpot <- potlist[[i]]
    
    t1 <- ctabmarg(cpot, cvpa)
    t1 <- permctab(t1, cvpa)
    cptlist[[j]] <- t1
  }
  
  for (nid in 1:length(elorder)){

    cnode    <- elorder[nid]
    cstates  <- nodeStates(object)[cnode]
    p        <- as.data.frame(cptlist[[cnode]])
    pa  <- vpa[[cnode]][-1]    
    ### print(cnode); ### print(cstates); ###print(p)

    ## Get started
    ##
    if (nid==1){
      if (!is.null(seed)){
        set.seed(seed)
        seedscale <- round(rgamma(1,10)+2)
        foo       <- seed*seedscale; set.seed(foo)
      }
      #print(p$potential)
      ns    <- as.numeric(rmultinom(1, size = nsim, prob=p$potential))
      wdata <- p
      wdata$potential <- ns
      ## print("Start..."); print(wdata)

    } else {
      if (length(pa)){
        condform <- as.formula(paste("~",paste(pa, collapse='+')))
        pl   <- splitBy(condform, p)
        gid  <- attr(pl, "groupid")
      }
      
      res    <- as.list(rep(NA, nrow(wdata)))
      wdata2 <- wdata;
      wdata2$potential <- NULL
      
      for (i in 1:nrow(wdata)){
        aa<-wdata[i,pa,drop=FALSE]
        if (ncol(aa)){
          argh <- apply(gid,1,"==", aa)
          if (is.null(dim(argh))){
            idx <- which(argh)
          } else { 
            idx <- which(apply(argh,2,all))
          }
          
          p <- pl[[idx]]
        } 
        nsim <- wdata[i,"potential"]
        pot <- p$potential

        if (!is.null(seed)){
          set.seed(seed)
          seedscale <- round(rgamma(1,10)+2)
          foo <- seed*seedscale; set.seed(foo)
        }
#        print(pot)
#        print(nsim)
        if (sum(pot)==0)
          pot[] <- 1
        ns <- as.numeric(rmultinom(1, size = nsim, prob=pot))
        ns <- cbind(expand.grid(cstates),potential=ns)
        res[[i]] <- merge(wdata2[i,,drop=FALSE],ns)
      }
      
      wdata <- do.call("rbind", res)
      wdata <- wdata[wdata$potential>0,]
    }
  }

  rownames(wdata) <- 1:nrow(wdata)
  names(wdata)[length(names(wdata))] <- "Freq"
  
  attr(wdata, "seed") <- RNGstate
  wdata
}



