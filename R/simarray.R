

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
  p <- tableSlice(x, r, index)
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

