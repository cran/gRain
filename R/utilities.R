
.printList <- function(x){
  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x, 1:length(x))
  return()
}

## Represent list of sets in a matrix...
##
.as.setmat <- function(glist,vn=unique(unlist(glist))){
  amat <- matrix(0, nr=length(glist), nc = length(vn))
  colnames(amat) <- vn
  
  for (i in 1:length(glist)){
    amat[i, glist[[i]]] <- 1
  }
  amat
}


.formula2char <- function(x){
  if (class(x)=="formula"){
    ## In principle all this can be replaced by all.vars(); but I am not sure if
    ## all.vars() is guaranteed to preserve order.
    x2 <- deparse(x)
    x2 <- gsub("~","",x2)
    x2 <- unlist(strsplit(x2,"[~\\+\\|]"))
    x2 <- gsub(" +","",x2)
    x2
  } else {
    x
  }
}






