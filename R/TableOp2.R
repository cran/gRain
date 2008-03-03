
## Multiply/divide two potentials
##
ctabmult <- function(t1,t2){
  ctabop(t1,t2,"*")
}

ctabdiv <- function(t1,t2){
  ctabop(t1,t2,"/")
}

# ctabop <- function(t1, t2, op="*"){
#   ctabopx(t1,t2,op)
# }

ctabop <- function(t1, t2, op="*"){

  nam1    <- t1$varNames
  levn1   <- t1$nLevels
  pot1    <- t1$values
  levels1 <- t1$levels
  
  nam2    <- t2$varNames
  levn2   <- t2$nLevels
  pot2    <- t2$values
  levels2 <- t2$levels
  
  ## If names of t2 are not in names of t1, then expand t1
  ## to make if so, i.e. so that varNames(t2) are in varNames(t1)
  ##
  idx       <- match(nam2, nam1)
  if (any(is.na(idx))){
    augnames  <- nam2[is.na(idx)]
    auglevn   <- levn2[is.na(idx)]
    auglevels <- levels2[is.na(idx)]
    pot1 <- rep(pot1, prod(auglevn))
    nam1 <- c(nam1, augnames)
    levn1 <- c(levn1, auglevn)
    levels1 <- c(levels1, auglevels)
  }    
  ##pot1 <- array(pot1, levn1)

  dim(pot1) <- levn1
  ## If t1 has names a, b, c and t2 has names c, a then permute
  ## t1 to have names c, a, b. Then from here multiplication is 
  ## straight forward.
  ##
  idx     <- match(nam2, nam1)
  
  permidx    <- c(idx, setdiff(1:length(nam1),idx))
  pot1  <- aperm(pot1, permidx)
  switch(op,
         "*"={
           pot1  <- pot1 * pot2
           dim(pot1) <- NULL
         },
         "/"={
           pot1  <- pot1 / pot2
           dim(pot1) <- NULL
           pot1[!is.finite(pot1)] <- 0
         }
         )
  
  t3  <- ctabFRAGILE(nam1[permidx],levels1[permidx],pot1)
  t3
}


## Marginalize a potential onto a set of variables
## (which must be in the potentials domain)
##
ctabmarg <- function(t1,marg,normalize=FALSE){

  if (length(marg)==1 && is.na(marg)){
    return(sum(t1$values))
  }

  perm <- c(setdiff(t1$varNames, marg),marg)
  t1 <- permctab(t1,perm)
  nc <- prod(sapply(t1$levels[marg],length))
  mat  <- matrix(t1$values, nc=nc)

  m <- colSums(mat)

  if  (normalize)
    m <- m/sum(m)
  ##ctab(marg, t1$levels[marg], m)
  ctabFRAGILE(marg, t1$levels[marg], m)  
}



## Permute a ctab according to variable ordering in perm
##
permctab <- function(tab, perm){
  permidx <- match(perm, varNames(tab))
  a <- array(tab$values, tab$nLevels)
  #print(a); print(perm); print(varNames(tab)); print(permidx)
  a <- aperm(a, permidx)
  dim(a) <- NULL
  ctab(perm, tab$levels[permidx], values=a)
}


## Normalize on the first variable.
## Kan gøres bedre...
##
normalizectab <- function(t1){
  lev <- t1$levels
  values <- t1$values
  vlev  <- length(lev[[1]])
  palev <- sapply(lev[-1],length)
  if(length(palev)>0)
    for (i in 0:(prod(palev)-1)){
      idx <- i*vlev + 1:vlev
      vv <- values[idx]
      svv <- sum(vv)
      if (svv!=0)
        values[idx] <- vv/svv
      
    } else {
      svalues <- sum(values)
      if (svalues!=0)
        values <- values / svalues
    }
  t1$values <- values
  t1
}


# ctabmargx <- function(t1,marg,normalize=FALSE){
#   if (length(marg)==1 && is.na(marg)){
#     return(sum(t1$values))
#   }
#   a <- array(t1$values, t1$levn)
#   margidx <- match(marg, t1$names)
#   m <- apply(a, margidx, sum)
#   dim(m) <- NULL
#   if  (normalize)
#     m <- m/sum(m)
#   ctab(marg, t1$levels[marg],m)  
# }
