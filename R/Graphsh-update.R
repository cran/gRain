

updateugsh <- function(x, add=NULL){
  if (!is.null(add)){
    if (is.list(add)){
      add <- lapply(add, sort)
      add <- unlist(lapply(add,names2pairs),recursive=FALSE)
    } else {
      add <- sort(add)
      add <- names2pairs(add)
    }
    newed <- unique(c(edges(x), add))
    x@edges <- newed
    x@nodes <- unique(unlist(newed))
  }  
  x
}
