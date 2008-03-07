
setClassUnion("listOrNULL", c("list",     "NULL"))
setClassUnion("charOrNULL", c("character","NULL"))

setClass("graphsh",
         representation(gens="listOrNULL",
                        nodes="charOrNULL",
                        edges="listOrNULL",
                        short="logical"))

setClass("ugsh",   contains="graphsh")
setClass("dagsh",  contains="graphsh")


### ***************************************************

setMethod("show", signature(object = "ugsh"),         
          function(object) {
            x <- object
            cat("Undirected graph\n")
            if (!isTRUE(x@short)){
              n <- paste(nodes(x),collapse=' ')
              e <- paste(sapply(edges(x), paste, collapse='~'),collapse=' ')
              cat("Nodes:", n, "\n")
              cat("Edges:", e, "\n")
            }
            ##cat("slots:", paste(slotNames(x)), "\n")
          })

setMethod("show", signature(object = "dagsh"), 
          function(object) {
            x <- object
            cat("Directed graph\n")
            if (!isTRUE(x@short)){
              n <- paste(nodes(x),collapse=' ')
              e <- paste(sapply(edges(x), paste, collapse='<-'),collapse=' ')
              cat("Nodes:", n, "\n")
              cat("Edges:", e, "\n")
            }
            ###cat("slots:", paste(slotNames(x)), "\n")            
          })

### ***************************************************

setMethod("edges", signature(object = "ugsh"),
          function(object, which) {
            object@edges
          })

setMethod("edges", signature(object = "dagsh"),
          function(object, which) {
            object@edges
          })

setMethod("nodes", signature(object = "graphsh"),
          function(object, ...) {
            object@nodes
          })

### ***************************************************


newugsh <- function(x=NULL, ..., short=FALSE){
  x <- lapply(x, function(a){
    if (class(a)=="formula"){  
      a <- gsub("~","",deparse(a))
      unlist(strsplit(gsub(" +","",a) ,"\\+"))
    } else {
      a
    }
  })
  
  x   <- lapply(x, sort)
  x   <- unlist(lapply(x, names2pairs, sort=TRUE), recursive=FALSE)
  x   <- unique(x)
  nodes   <- unique(unlist(x))
  xLength <- sapply(x,length)
  edges   <- x[xLength==2]
  if (length(edges)==0)
    edges <- NULL
  value <- new("ugsh", gens=NULL, nodes=nodes, edges=edges,short=short)
  return(value)
}

newdagsh <- function(x,...,short=FALSE){

  x <- lapply(x, function(a){
    if (class(a)=="formula"){  
      a <- gsub("~","",deparse(a))
      unlist(strsplit(gsub(" +","",a) ,"\\+"))
    } else {
      a
    }
  })

  x <- lapply(x, function(xx) names2pairs(xx[1],xx[-1], sort=FALSE))
  x <- unlist(x,recursive=FALSE)
  x <- unique(x)
  nodes   <- unique(unlist(x))
  
  xLength <- sapply(x,length)
  edges   <- x[xLength==2]
  if (length(edges)==0)
    edges <- NULL

  value <- new("dagsh", gens=NULL, nodes=nodes, edges=edges, short=short)

  if(!is.null(eliminationOrder(value)))
    return(value)
  else
    return(NULL)
}

newug <- function(..., short=FALSE){
  newugsh(list(...),short=short)
}

newdag <- function(...,short=FALSE){
  newdagsh(list(...),short=short)
}


