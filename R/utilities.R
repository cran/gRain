
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






