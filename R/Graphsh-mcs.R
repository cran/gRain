##
## Maximum Cardinality Search
## October, 2007
##
## Returns perfect ordering if it exists and NULL otherwise
##

# mcsOLD <- function(ug, root=NULL){
#   unumneigh   <- nodes(ug)
#   ed          <- edges(ug)
#   numneigh    <- NULL

#   if (!is.null(root))
#     unumneigh <- c(root, setdiff(unumneigh, root))
  
#   nnvec <- rep(0,length(unumneigh))   ## Gives the number of numbered neighbours.
#   names(nnvec) <- unumneigh

#   amat <- adjmat(ug)
  
#   ## Start somewhere
#   cv <- unumneigh[1]
  
#   is.perfect <- TRUE
#   repeat{
#     numneigh  <- c(numneigh, cv)
#     unumneigh <- setdiff(unumneigh, cv)
  
#     nb <- neigh(ug, cv)
#     anumne   <- intersect(nb, numneigh) ## Already numbered neighbours
#     ##print(dput(anumne))
#     ##iscomp   <- is.completeset(anumne, ed)
#     iscomp  <- is.completesetMAT(anumne, amat)
#     ##print(c(iscomp,iscomp2))
    
#     if (!iscomp){
#       is.perfect <- FALSE
#       break()
#     }
#     #iscomp2 <- is.completeset(anumne, ed)    
#     nnvec[nb] <- nnvec[nb]+1
  
#     cv <- names(which.max(nnvec[unumneigh]))
#     if (!length(unumneigh))
#       break()
#   }

#   if (is.perfect)
#     return(numneigh)
# }


mcs <- function(ug, root=NULL){
  
  is.perfect <- TRUE
  amat  <- adjmat(ug)
  vn    <- colnames(amat)
  
  if (!is.null(root)){
    vn <- c(root, setdiff(vn, root))
    amat <- amat[vn,vn]  
  }
  
  i <- 1
  mcmat   <- matrix(c(1,0,0), nr=3, nc=length(vn))
  dimnames(mcmat) <- list(c("U","L","nne"), vn)
  
  repeat{
    
    Unames <- vn[mcmat["U",]==1]  ## Currently unlabled vertices
    idx <- which.max( mcmat["nne", Unames] )
    v <- Unames[idx]
    #print(mcmat)
    #print(v)
    nev <- vn[which(amat[v,])]
    set <- intersect(nev, names(which(mcmat["L",]>=1)))
    if (!is.completesetMAT(set, amat)){
      is.perfect <- FALSE
      break()
    } else {
      mcmat["U",v] <- 0
      mcmat["L",v] <- i
      isect <-intersect(nev, vn[mcmat["U",]>=1])
      if (length(isect)){
        mcmat["nne",isect] <-     mcmat["nne",isect] + 1
      }
      i <- i + 1
      if (i==length(vn)+1)
        break()
    }
  }
  if (is.perfect)
    return(vn[order(mcmat["L",])])
  else
    return(NULL)
}

#colnames(mcmat)[order(mcmat["L",])]


