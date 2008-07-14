##
## plot (gRain)
##

"plot.cpt-gmInstance" <- function(x, ...){
  .plot.graphsh(x$dag)
}

"plot.dag-gmInstance" <- function(x, ...){
  .plot.graphsh(x$dag)
}

"plot.ug-gmInstance" <- function(x, ...){
  .plot.graphsh(x$ug)
}

plot.compgmInstance <- function(x, ...){
  .plot.graphsh(x$rip$tug)
}

plot.ugsh <- function(x, ...){
  .plot.graphsh(x)
}

plot.dagsh <- function(x, ...){
  .plot.graphsh(x)
}


.plot.graphsh <- function(graph){

  if (!require("Rgraphviz")){
    cat("The Rgraphviz package (from Bioconductor) must be installed to display the models\n")
    return()
  }

  plot(as.graphNEL(graph))
  return(invisible(graph))
}

########################################################################





# .plot.graphsh <- function(graph){

#   if (length(graph)==0)
#     return(NULL)


#   edges <- edges(graph)
#   nodes <- nodes(graph)

#   nAttrs <- list()
#   nodeColours        <- rep("yellow", length(nodes))
#   names(nodeColours) <- nodes
#   nAttrs$fillcolor   <- nodeColours
  
#   edgeColours        <- rep("blue",length(edges))
#   names(edgeColours) <- sapply(edges, function(ee) {
#     estr <- paste(ee[1],"~",ee[2],sep='');
#     estr
#   })

#   eAttrs             <- list(color=edgeColours)

#   V   <- nodes

#   edL <- vector("list", length=length(V))
#   names(edL) <- V
#   nv <- 1:length(V)
#   names(nv) <- V    
#   ed <- edges

#   if (class(graph)[1]=="ugsh"){
#     for (i in 1:length(V)){
#       idx <- sapply(ed, function(x) is.element(V[i],x))
#       e   <- setdiff (unlist(ed[idx]),V[i])
#       edL[[V[i]]] <- list(edges=nv[e])
#     }
#     edL<- edL[sapply(edL,length)>0]
#     G <- new("graphNEL", nodes=V,edgeL=edL,edgemode='undirected')
#   } else {
#     ed <- do.call("rbind",ed)
    
#     for (i in 1:length(V)){
#       #print(V[i])
#       idx <- which((V[i] == ed[,2]))
#       #print(idx)
#       if (length(idx)){
#         e <- ed[idx,1]
#         edL[[V[i]]] <- list(edges=e)
#       } else {
#         edL[[V[i]]] <- list()
#       }
#     }
#     G <- new("graphNEL", nodes=V,edgeL=edL,edgemode='directed')
#   }

#   ##
#   plot(G, "neato", nodeAttrs = nAttrs, edgeAttrs = eAttrs)

#   return(invisible(graph))
# }











  

#   if (class(graph)[1]=="ugsh"){
#     G <- new("graphNEL", nodes=nodes,edgemode='undirected')
#   } else {
#     G <- new("graphNEL", nodes=nodes,edgemode='directed')
#   }

#   if (length(edges)>0){
#     for (i in 1:length(edges)){
#       ee <- rev(edges[[i]]);  ##  cat("Adding edge:", paste(ee),"\n")
#       ##G <- addEdge(ee[1], ee[2] , G, weight=1)
#       G <- addEdge(ee[1], ee[2], G)
#     }
#   }









# plotOld <- function(graph){

#   if (length(graph)==0)
#     return(NULL)


#   edges <- edges(graph)
#   nodes <- nodes(graph)

#   nAttrs <- list()
#   nodeColours        <- rep("yellow", length(nodes))
#   names(nodeColours) <- nodes
#   nAttrs$fillcolor   <- nodeColours
  
#   edgeColours        <- rep("blue",length(edges))
#   names(edgeColours) <- sapply(edges, function(ee) {
#     estr <- paste(ee[1],"~",ee[2],sep='');
#     estr
#   })

#   eAttrs             <- list(color=edgeColours)


#   if (class(graph)[1]=="ugsh"){
#     G <- new("graphNEL", nodes=nodes,edgemode='undirected')
#   } else {
#     G <- new("graphNEL", nodes=nodes,edgemode='directed')
#   }

#   if (length(edges)>0){
#     for (i in 1:length(edges)){
#       ee <- rev(edges[[i]]);  ##  cat("Adding edge:", paste(ee),"\n")
#       ##G <- addEdge(ee[1], ee[2] , G, weight=1)
#       G <- addEdge(ee[1], ee[2], G)
#     }
#   }
#   plot(G, "neato", nodeAttrs = nAttrs, edgeAttrs = eAttrs)

#   return(invisible(graph))
# }

