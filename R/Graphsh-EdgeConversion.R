#######################################################################

# # Conversion of edges between different formats.

# # glist  : a list of generators ('undirected')

# # adjmat : A boolean matrix
# #          (directed edges or undirected edges; no mixing)

# # nel    : NEL (node-edge-list) format as used in 'graph' package
# #          (directed edges or undirected edges; no mixing)

# # pl     : Pair-list format as used in 'gRain' package
# #          (no distinction between directions on edges)

# #          | glist    adjmat    pl       nel
# # ============================================
# # glist    |            ok      ok       ok
# # adjmat   |                             ok
# # pl       |            ok
# # nel      |                    ok

# # Examples:

# glist <- lapply(list(c(1,2,3),c(2,3,4),c(4,5),6), as.character)

# ## Convert generators to NEL 
# nel   <- glist2nel(glist)
# ## Convert generators to PL 
# pl    <- glist2pl(glist)

# ## Convert generators to adjmat 
# amat <- glist2adjmat(glist)

# plist <- lapply(list(c(1,2),c(1,3),c(2,4),c(3,4)), as.character)

# ## Convert PL to adjmat
# amat1 <- pl2adjmat(plist, edge="undirected")
# amat2 <- pl2adjmat(plist, edge="directed")

# ## Convert adjmat to NEL
# nel1  <- adjmat2nel(amat1)
# nel2  <- adjmat2nel(amat2)

#######################################################################







## Convert NELedges to list of pairs
##
nel2pl <- function(ed){
  ed2 <- do.call("rbind",mapply(function(n,x){cbind(n,x)}, names(ed), ed))
  ed2 <- split(ed2,row(ed2),drop=TRUE)
  ed2 <- maximalSet(ed2)
  ed2
}

## Convert generator list to NEL edges 
##
glist2nel <- function(glist,vn=unique(unlist(glist))){
  am<-setmat2adjmat(as.setmat(glist,vn))
  amL <- split(am, row(am))
  ans <- lapply(amL, function(a) vn[a==1])
  names(ans) <- vn
  ans
}

## Convert generator list to PAIR edges
##
glist2pl <- function(glist){
  ans <- lapply(glist, names2pairs)
  ans <- unlist(ans, rec=FALSE)
  ans <- ans[sapply(ans,length)>1]
  ans <- remove.redundant(ans)
  ans
}


## Convert pl edges to adjmat
##
pl2adjmat <- function(glist, vn=unique(unlist(glist)), edgemode="undirected"){
  amat <- matrix(0, nc=length(vn), nr=length(vn))
  dimnames(amat) <- list(vn,vn)
  for (ii in 1:length(glist)){
    amat[glist[[ii]][2], glist[[ii]][1]] <- 1
  }
  if (edgemode=="undirected")
    amat <- amat + t(amat)
  amat
}

adjmat2nel <- function(amat, vn=colnames(amat)){
  ans <- lapply(split(amat, row(amat)), function(a) which(a>0))
  ans <- lapply(ans, function(a) vn[a])
  
  vn <- colnames(amat)
  names(ans) <- vn[1:nrow(amat)]
  ans
}


glist2adjmat <- function(glist, vn=unique(unlist(glist))){
  amat <- matrix(0, nc=length(vn), nr=length(vn))
  dimnames(amat) <- list(vn,vn)

  for (ii in 1:length(glist)){
    gg <- glist[[ii]]
    lgg <- length(gg)
    if (lgg>1){
      for (jj in 1:(lgg-1)){
        for (kk in (jj+1):lgg){
          amat[gg[jj],gg[kk]] <- amat[gg[kk],gg[jj]] <- 1
        }
      }
    }
  }
  amat
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


## Convert a set matrix to adjacency matrix
##
setmat2adjmat <- function(smat){
  amat <- matrix(0, nr=ncol(smat), nc=ncol(smat))
  nr <- nrow(amat)
  dimnames(amat) <- list(colnames(smat),colnames(smat))
  for (kk in 1:nrow(smat)){
    r <- smat[kk,]
    for (ii in 1:(nr-1)){
      if (r[ii]==1){
        for (jj in (ii+1):nr){
           if (r[jj]==1){
              amat[ii,jj] <- amat[jj,ii] <- 1      
           }
        }
      }
    }
  }
  amat
}

