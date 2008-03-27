
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
            cat("Undirected graph with",length(nodes(x)),"nodes and",length(edges(x)),"edges\n")
            if (!isTRUE(x@short)){
              n <- paste(nodes(x),collapse=' ')
              e <- paste(sapply(edges(x), paste, collapse='~'),collapse=' ')
              #cat("Nodes:", n, "\n")
              #cat("Edges:", e, "\n")
              #cat("Number of nodes:", length(nodes(x)),
              #    "Number of edges:", length(edges(x)),"\n")
            }
            ##cat("slots:", paste(slotNames(x)), "\n")
          })

setMethod("show", signature(object = "dagsh"), 
          function(object) {
            x <- object
            #cat("Directed graph\n")
            cat("Directed graph with",length(nodes(x)),"nodes and",length(edges(x)),"edges\n")
            if (!isTRUE(x@short)){
              n <- paste(nodes(x),collapse=' ')
              e <- paste(sapply(edges(x), paste, collapse='<-'),collapse=' ')
              #cat("Nodes:", n, "\n")
              #cat("Edges:", e, "\n")
              #cat("Number of nodes:", length(nodes(x)),
              #    "Number of edges:", length(edges(x)),"\n")
            
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
  isForm <-sapply(x, inherits, "formula")
  
  flist <- x[isForm]
  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    V     <- rownames(attr(tt,"factors"))
    list(glist=glist,V=V)
  }) 
  
  gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
  V     <- lapply(ans, "[[", "V")
  
  gset <- c(gset, x[!isForm])
  V    <- unique(unlist(c(V, x[!isForm])))
  
  ed   <- unlist(lapply(gset, names2pairs, sort=FALSE), recursive=FALSE)
  ed   <- unique(lapply(ed, sort))
  ed   <- ed[sapply(ed,length)==2]
  
  if (length(ed)==0)
    ed <- NULL
  value <- new("ugsh", gens=NULL, nodes=V, edges=ed,short=short)
  value
}


newdagsh <- function(x=NULL, ..., short=FALSE){
  isForm <-sapply(x, inherits, "formula")
  
  flist <- x[isForm]
  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    V     <- rownames(attr(tt,"factors"))
    list(glist=glist,V=V)
  }) 
  
  gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
  V     <- lapply(ans, "[[", "V")
  
  gset <- c(gset, x[!isForm])
  V    <- unique(unlist(c(V, x[!isForm])))

  gset <- lapply(gset, function(xx) names2pairs(xx[1],xx[-1], sort=FALSE))
  gset <- unlist(gset,recursive=FALSE)
  gset <- unique(gset)

  ed   <- gset[sapply(gset,length)==2]
 
  if (length(ed)==0)
    ed <- NULL
  value <- new("dagsh", gens=NULL, nodes=V, edges=ed,short=short)

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




# newugsh <- function(x=NULL, ..., short=FALSE){
#   x <- lapply(x, function(a){
#     if (class(a)=="formula"){  
#       a <- gsub("~","",deparse(a))
#       unlist(strsplit(gsub(" +","",a) ,"\\+"))
#     } else {
#       a
#     }
#   })
  
#   x   <- lapply(x, sort)
#   x   <- unlist(lapply(x, names2pairs, sort=TRUE), recursive=FALSE)
#   x   <- unique(x)
#   nodes   <- unique(unlist(x))
#   xLength <- sapply(x,length)
#   edges   <- x[xLength==2]
#   if (length(edges)==0)
#     edges <- NULL
#   value <- new("ugsh", gens=NULL, nodes=nodes, edges=edges,short=short)
#   return(value)
# }




# newdagsh <- function(x,...,short=FALSE){

#   x <- lapply(x, function(a){
#     if (class(a)=="formula"){  
#       a <- gsub("~","",deparse(a))
#       unlist(strsplit(gsub(" +","",a) ,"\\+"))
#     } else {
#       a
#     }
#   })

#   x <- lapply(x, function(xx) names2pairs(xx[1],xx[-1], sort=FALSE))
#   x <- unlist(x,recursive=FALSE)
#   x <- unique(x)
#   nodes   <- unique(unlist(x))
  
#   xLength <- sapply(x,length)
#   edges   <- x[xLength==2]
#   if (length(edges)==0)
#     edges <- NULL

#   value <- new("dagsh", gens=NULL, nodes=nodes, edges=edges, short=short)

#   if(!is.null(eliminationOrder(value)))
#     return(value)
#   else
#     return(NULL)
# }


