
moralize <- function(dag){
  amat <- amat2 <- adjmat(dag)
  for(k in 1:ncol(amat)){
    idx <- which(amat[,k])
    lenidx <- length(idx)
    if (lenidx>1){ 
      for (i in 1:(lenidx-1)){
        for (j in (i+1):lenidx) {
#          if (!amat[idx[i],idx[j]]){
            #cat("Node:",colnames(amat)[k],
            #    " Adding:", paste(names(c(idx[i],idx[j])), sep=''),"\n")
            amat2[idx[i],idx[j]] <- TRUE
#          }
        }
      }
    }
  }
  
  arrind <-  which(amat2, arr.ind=TRUE)
  if (nrow(arrind)>0){
    arrindc <- arrind
    storage.mode(arrindc) <- "character"
    dimnames(arrindc) <- NULL
    vn <- colnames(amat2)
    for (i in 1:nrow(arrind)){
      arrindc[i,] <- c(vn[arrind[i,]])
    }
    ed <- unlist(apply(arrindc, 1, list),recursive=FALSE)
  } else {
    ed <- NULL
  }
  ##newugsh(c(nodes(dag),ed))
  value <- new("ugsh", gens=NULL, nodes=nodes(dag), edges=ed)
  value
}









checkdag <- function(dag, cptlist){

  cat("..Checking DAG for acyclicity (to be done)\n")
  cat("..Checking that conditionals are defined on DAG\n")
  ##cptnamevec <- lapply(cptlist, cptnames)
  cptnamevec <- lapply(cptlist, varNames)
  value <- sapply(cptnamevec,
                  function(x){
                    v <- subsetofList(x, dag)
                    if (!v){
                      cat ("cpt",x, "not defined on DAG\n")     
                      value <- FALSE
                    }
                    return(v)
                  }
                  )

  return(!any(!value))
}
