
.defaultControl <- function(){
  list(timing=0)
}

.printList <- function(x){
  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x, 1:length(x))
  return()
}

## Represent list of sets in a matrix...
##
as.setmat <- function(glist,vn=unique(unlist(glist))){
  amat <- matrix(0, nr=length(glist), nc = length(vn))
  colnames(amat) <- vn
  
  for (i in 1:length(glist)){
    amat[i, glist[[i]]] <- 1
  }
  amat
}



dag2cptspec <- function(dag, gmData, smooth=0){
  vparlist <- vpar(dag)
  ans      <- lapply(vparlist, cpt2, gmData=gmData, smooth=smooth, normalize='first')
  .cptspec(ans)
}


ug2potspec <- function(ug, gmData, rip, smooth=0){
  cliq    <- rip$cliques
  seps    <- rip$separators

  ans <- as.list(rep(NA, length(cliq)))
  for (i in 1:length(cliq)){  
    currc <- cliq[[i]]
    currs <- seps[[i]]

    #cat("currc:", paste(currc, sep=''),"\n")
    #cat("currs:", paste(currs, sep=''),"\n")
        
    currtab <- cpt2(currc, gmData=gmData, smooth=smooth, normalize='none')

    if (!all(is.na(currs))){
      mtab    <- tableMarginPrim(currtab, currs)
      currtab <- tableOp(currtab, mtab, "/")
    } else {
      ##currtab$values <- currtab$values/sum(currtab$values)
      currtab <- currtab/sum(currtab) ## BRIS
    }
    ans[[i]] <- currtab
   }  

  ##print("ug2potspec - DONE")
  ##ans   <- lapply(cliq, cpt2, gmData=gmData, smooth=0, normalize=TRUE)
  #ans   <- lapply(cliq, cpt2, gmData=gmData, smooth=0, normalize='none')

  ##print(ans)
  return(ans)
}





extractPotentials <- function(x, cliq, seps=NULL, smooth=0){
  UseMethod("extractPotentials")

}

extractPotentials.table <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq <- cliq[[ii]]
    sp <- seps[[ii]]
    t.cq <- tableMargin(x, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp <- tableMargin(t.cq, sp)
      ans[[ii]] <- .tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}

extractPotentials.data.frame <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq <- cliq[[ii]]
    sp <- seps[[ii]]
    t.cq <- table(x[,cq]) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp <- tableMargin(t.cq, sp)
      ans[[ii]] <- .tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}


.formula2character <- function(x){
  if (class(x)=="formula"){
    x2 <- deparse(x)
    x2 <- unlist(strsplit(x2,"[~\\+]"))
    x2 <- gsub(" +","",x2)
    x2 <- x2[as.logical(nchar(x2))]
  } else {
    x
  }
}

ug2dag <- function(ug){
  m <- mcs(ug)
  if (length(m)==0)
    return(NULL)
  adjList <- adj(ug, m)
  vparList <- vector("list",length(m))
  names(vparList) <- m
  
  ii <- 2
  vparList[[1]] <- m[1]
  for (ii in 2:length(m)){
    vparList[[ii]] <- c(m[ii],intersectPrim(adjList[[ii]], m[1:ii]))
  }
  
  dg <- dagList(vparList)
  dg
}






## ug2dag <- function(ug){
##   m     <- MCS(ug)
##   #m     <- mcs(as.adjmat(ug))
##   if (is.null(m))
##     return(NULL)
  
##   dired <- ed  <- edges(ug)
##   if (!is.null(ed)){
##     for (i in 1:length(ed)){
##       cedge <- ed[[i]]
##       dired[[i]] <- m[rev(sort(match(cedge,m)))]
##     }
##     dired <- c(m[1], dired)
##     #dag<-newdagsh(dired)
##     dag<-dagList(dired)
##   } else {
##     #dag <- newdagsh(as.list(nodes(ug)))
##     dag <- dagList(as.list(nodes(ug)))
##   }
  
##   return(dag)
## }




## Marginalize array onto margin
## FIXME: Remove this...
tableMarginPrim <- function(t1, margin, normalize=FALSE){
  if (missing(margin) || (length(margin)==1 && is.na(margin))){
    return(sum(as.numeric(t1)))
  }
  vn    <- names(dimnames(t1))
  idx   <- match(margin,vn)
  x     <- apply(t1, idx, sum)
  if (normalize)
    x <- x/sum(x)
  att           <- attributes(t1)
  attributes(x) <- list(dim=att$dim[idx], dimnames=att$dimnames[idx], class="ptable")
  x
}





