printlist <- function(x,d=0) UseMethod("printlist")

printlist.default <- function(x,d=0){
  paste("(", paste(x,collapse=' '),")",sep='')
}

printlist.list <- function(x,d=0){
  tmp     <- unlist(lapply(x, printlist, d+2),recursive=FALSE)
  prefix  <- as.list(c("(",rep(" ",length(tmp)-1)))
  posfix  <- as.list(c(rep(" ",length(tmp)-1),")"))
  as.list(mapply(function(l,x,r) {paste(l,x,r,sep='')}, prefix, tmp, posfix))
}

splitVec <- function(val, lev) UseMethod("splitVec")

splitVec.default <- function(val, lev){
  m    <- matrix(val,ncol=lev)
  cval <- unlist(apply(m,2,list),recursive=FALSE)
  cval
}

splitVec.list <- function(val, lev){
  lapply(val, splitVec, lev)
}

