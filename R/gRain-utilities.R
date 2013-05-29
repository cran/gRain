
.printList <- function(x){
  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x, 1:length(x))
  return()
}


.formula2char <- function(f) {
	unlist(rhsf2list(f))
}






