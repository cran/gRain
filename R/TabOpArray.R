####
#### Implementation of potentials based on arrays
#### Bristol, March 2008
####

## Create ptab object (really an array)
##
ptab <- function(varNames, nLevels, values=1, normalize=c("none","first","all"),
                 smooth=0 
                 ){

  if (is.list(nLevels)){
    dimnames = nLevels
    names(dimnames) <- varNames
    nLevels  = sapply(dimnames, length)
  } else {
    dimnames=makeDimNames(varNames, nLevels)
  }


  normalize <- match.arg(normalize, choices=c("none","first","all"))

  ##cat("ptab:", smooth, "\n")
  if (!identical(smooth,0)){
    values[values==0] <- smooth
  }

  ans <- array(values, dim=nLevels, dimnames=dimnames)

  switch(normalize,
    "first"={
      if (length(dim(ans))>1){
        marg  <- 2:length(dim(ans))
        ma    <- apply(ans, marg, sum)
        ans   <- sweep(ans, marg, ma, "/")
      } else {
        ans <- ans / sum(ans)
      }
    },
    "all"={ans <- ans / sum(ans)
    },
    "none"={}
    )

  class(ans) <- "ptab"
  return(ans)
}

## Create list with dimension names
##
makeDimNames <- function(varNames, nLevels,sep=''){
  if (missing(varNames) || is.null(varNames))
    return(lapply(nLevels, seq))
  lev <- lapply(nLevels, function(a) c(1:a))
  mapply(function(n,v) paste(n,v,sep=sep), varNames, lev, SIMPLIFY=FALSE)
}

## Coercion
##
as.ptab  <- function(x, ...){
  values <- x
  if (!inherits(values, c("array","matrix","integer","double"))){
    stop("arg must be array, matrix, integer or double\n")
  }
  if (is.null(dimnames(values))){
    if (!is.null(dim(values)))
      nLevels <- dim(values)
    else 
      nLevels <- length(values)
    varNames <- paste("V", 1:length(nLevels),sep='')
    dimnames <- makeDimNames(varNames, nLevels)
    ans <- array(values, dim = nLevels, dimnames = dimnames)
    class(ans) <- "ptab"
  } else {
    ans <- values
    class(ans) <- "ptab"
  }
  return(ans)
}  




## Find permutation of variables in set2 such that those in set1 are either
## to the far right or far left
##
permidx <- function(set1, set2, direction="right"){
   idx <- 1:length(set2)
   i   <- match(set1, set2)
   switch(direction,
    "right"={c(idx[-i],i)},
    "left" ={c(i,idx[-i])}
  )
}

## Multiply two arrays
##
arrayop <- function(t1, t2, op = "*"){
  levels1 <- dimnames(t1)
  levels2 <- dimnames(t2)
  
  vn1    <- names(levels1)
  vn2    <- names(levels2)
  
  lev1 <- sapply(levels1,length)
  lev2 <- sapply(levels2,length)
  
  idx       <- match(vn2, vn1)
    if (any(is.na(idx))){
      augnames  <- vn2[is.na(idx)]
      auglevn   <- lev2[is.na(idx)]
      auglevels <- levels2[is.na(idx)]
      pot1      <- rep(as.numeric(t1), prod(auglevn))
      vn1       <- c(vn1, augnames)
      lev1      <- c(lev1, auglevn)
      levels1   <- c(levels1, auglevels)
      dim(pot1) <- lev1
    } else {
      pot1 <- t1
    }
    
  perm  <- permidx(set1=vn2, set2=vn1,"left")
  if (op=="*"){
    pot1   <- as.numeric(aperm(pot1, perm)) * as.numeric(t2)
  } else {
    pot1   <- as.numeric(aperm(pot1, perm)) / as.numeric(t2)
    pot1[!is.finite(pot1)] <- 0
  }
  attributes(pot1) <- list(dim=lev1[perm], dimnames=levels1[perm], class="ptab")
  pot1
}




## Marginalize array onto marg
##
arraymarg <- function(t1, marg, normalize=FALSE){
  if (missing(marg) || (length(marg)==1 && is.na(marg))){
    return(sum(as.numeric(t1)))
  }
  vn    <- names(dimnames(t1))
  idx   <- match(marg,vn)
  x     <- apply(t1, idx, sum)
  if (normalize)
    x <- x/sum(x)
  att           <- attributes(t1)
  attributes(x) <- list(dim=att$dim[idx], dimnames=att$dimnames[idx], class="ptab")
  x
}

## Returns the subarray of x where the margin[i]'th index
## is index[i] for all i. If impose is a numerical value
## the entire array is returned but with non-matching values
## replaced by impose.
##
## Based on code kindly provided by Peter Green
##
subarray <- function (x, margin, index, impose) 
{

  if(is.null(margin)) return(x)
  d  <- dim(x)
  ld <- length(d)
  z  <- rep(TRUE,length(x))
  a  <- cumprod(d)/d[1]
  b  <- rev(a)
  
  for(i in 1:length(margin)) 
    {
      si<-margin[i]
      z<-z & index[i]==rep(rep(1:d[si], rep(a[si],d[si]) ), b[si])
    }
  dr<-d[(1:ld)[-margin]]
  if (!missing(impose) && is.numeric(impose)){
    x[!z] <- impose
    return(x)
  } else {
    newdn <- dimnames(x)[-margin]
    return(array(as.vector(x)[z],dr, dimnames=newdn))
  }
}

## Simulate n observations from the array x conditional on
## the variables in margin (a vector of indices) takes values
## given by index
##
## Based on code kindly provided by Peter Green
##
simarray <- function(x, n=1, margin, index){
  if(missing(margin)) {
      r <- NULL
      dr<-dim(x)
      cnames <- names(dimnames(x))
  } else { 
    r <- margin
    o <- (1:length(dim(x)))[-r]
    dr <- (dim(x))[o]
    cnames <- names(dimnames(x))[-r]
  }
  p <- subarray(x, r, index)
  ##print(p); print(r); print(index)
  samp <- sample(length(p),n,TRUE,p)
  ##print(cnames)
  ldr <-length(dr)
  cp  <-cumprod(c(1,dr[-ldr]))
  res <-matrix(0,n,ldr)
  for(j in 1:n) res[j,]<-1+((samp[j]-1)%/%cp)%%dr
  colnames(res) <- cnames
  res
}

## Accessors
##
varNames.array    <- function(x) names(attr(x,"dimnames"))
nLevels.array     <- function(x) dim(x)
valueLabels.array <- function(x) attr(x,"dimnames")

varNames.ptab    <- function(x) names(attr(x,"dimnames"))
nLevels.ptab     <- function(x) dim(x)
valueLabels.ptab <- function(x) attr(x,"dimnames")

print.ptab  <- function(x,...){
  class(x)<-NULL
  print(x)
  invisible(x)
}


ctab <- ptab
ctabop <- arrayop
ctabmarg <- arraymarg

