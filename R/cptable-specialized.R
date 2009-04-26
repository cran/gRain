ortable <- function(v, pa=NULL, pa1=c(TRUE,FALSE), pa2=c(TRUE,FALSE),
                levels
                ){

  vpa <- c(.formula2character(v),.formula2character(pa))
  if (length(vpa)!=3)
    stop("Must have exactly two parents!")
  lpa1 <- length(pa1)
  lpa2 <- length(pa2)
  z <- rep(pa1,lpa2) | rep(pa2,each=lpa1)
  pp <- array(c(z, !z),c(2,lpa2,lpa1))
  values <- as.numeric(aperm(pp, c(3,1,2)))
  ans <- list(vpa=vpa, values=values, normalize=FALSE, smooth=0, levels=levels)
  class(ans) <- "cptable"
  return(ans)
}



andtable <- function(v, pa=NULL, pa1=c(TRUE,FALSE), pa2=c(TRUE,FALSE),
                levels
                ){

  vpa <- c(.formula2character(v),.formula2character(pa))
  if (length(vpa)!=3)
    stop("Must have exactly two parents!")
  lpa1 <- length(pa1)
  lpa2 <- length(pa2)
  z <- rep(pa1,lpa2) & rep(pa2,each=lpa1)
  pp <- array(c(z, !z),c(2,lpa2,lpa1))
  values <- as.numeric(aperm(pp, c(3,1,2)))
  ans <- list(vpa=vpa, values=values, normalize=FALSE, smooth=0, levels=levels)
  class(ans) <- "cptable"
  return(ans)
}
