##
## Table operations
##

varNames.ctab    <- function(x) x$varNames
nLevels.ctab     <- function(x) x$nLevels
valueLabels.ctab <- function(x) x$levels


ctabFRAGILE <- function(varNames,levels,values){

  levn <- sapply(levels, length)
  v    <- list(varNames  = varNames,
               levels    = levels,
               values    = values,
               tab       = NULL,
               nLevels   = levn,
               ncells    = prod(levn)
               ) 
  class(v) <- "ctab";
  return(v)
}

ctab <- function(varNames, nLevels, values=1, normalize=c("none","first","all"), smooth=0){

  normalize <- match.arg(normalize, choices=c("none","first","all"))
  
  vv<- mapply(function(i,n){
    if (length(i)==1){
      if (is.numeric(i)){
        paste(n,1:i,sep='')
      } else { 
        cat("Error (in ctab function)\n")
      }
    } else {
      i
    }    
  }, nLevels, varNames, SIMPLIFY=FALSE)

  lev <- vv
  names(lev)   <- varNames
  levn         <- sapply(lev, length)
  levi         <- lapply(levn, function(i) 1:i)
  tabdim       <- prod(levn)
    
  ##if (is.null(values))
  ##  values <- 1

  if (length(values)<tabdim){
    values <- rep(values, tabdim)[1:tabdim]
  }

  if (!identical(smooth,0)){
    values[values==0] <- smooth
  }
  
  switch(normalize,
         "none"={
         },
         "first"={
           vlev  <- length(lev[[1]])
           palev <- sapply(lev[-1],length)
           if(length(palev)>0){
             for (i in 0:(prod(palev)-1)){
               idx <- i*vlev + 1:vlev
               vv  <- values[idx]
               svv <- sum(vv)
               if (svv!=0)
                 values[idx] <- vv/svv               
             }
           } else {
             svalues <- sum(values)
             if (svalues!=0)
               values <- values / svalues
           }
         },
         "all"={
           svalues <- sum(values)
           if (svalues!=0)
             values <- values / svalues
         })
  
  v   <- list(varNames   = varNames, levels=lev, values=values,
              tab     = NULL,
              nLevels = levn,
              ncells  = prod(levn)
              ) 
  class(v) <- "ctab";
  return(v)
}





print.ctab <- function(x,...){
  print(as.data.frame(x))
}

as.data.frame.ctab <- function(x, row.names, optional, ...){
  levn <- x$nLevels
  lev  <- x$levels
  value <- array(0, dim=levn, dimnames=lev)
  value <- as.data.frame.table(value,responseName='potential')
  value$potential<-x$values
  return(value)
}

as.double.ctab <- function(x, ...){
  if (length(x$varNames)>1)
    return(NULL)
  nam<-paste(unlist(x$levels))
  pot<-x$value
  names(pot)<-nam
  pot
}

as.ctab <- function(x) UseMethod("as.ctab")
as.ctab.numeric <- function(x){
  if (is.null(names(x)))
    lev <- list(1:length(x))
  else
    lev <- list(names(x))
  ctab("varName", lev, x)
}







# as.data.frame.ctab <- function(x, row.names, optional, ...){
#   tab  <- x
#   levn <- tab$levn
#   lev  <- tab$levels
#   value <- array(0, dim=levn, dimnames=lev)
#   value <- as.data.frame.table(value,responseName='potential')
#   value$potential<-tab$values
#   return(value)
# }


# ctabFRAGILE <- function(names,levels,values){

#   levn <- sapply(levels, length)
#   v    <- list(names  = names,
#                levels = levels,
#                values = values,
#                tab    = NULL,
#                levn   = levn,
#                ncells = prod(levn)
#                ) 
#   class(v) <- "ctab";
#   return(v)
# }

# ctab <- function(names, levels, values=1, normalize=c("none","first","all"), smooth=0){

#   normalize <- match.arg(normalize, choices=c("none","first","all"))
#   nam       <- names
#   lev       <- levels
  
#   vv<- mapply(function(i,n){
#     if (length(i)==1){
#       if (is.numeric(i)){
#         paste(n,1:i,sep='')
#       } else { 
#         cat("Error (in ctab function)\n")
#       }
#     } else {
#       i
#     }    
#   }, lev, nam, SIMPLIFY=FALSE)

#   lev <- vv
#   names(lev)   <- nam
#   levn         <- sapply(lev, length)
#   levi         <- lapply(levn, function(i) 1:i)
#   tabdim       <- prod(levn)
    
#   ##if (is.null(values))
#   ##  values <- 1

#   if (length(values)<tabdim){
#     values <- rep(values, tabdim)[1:tabdim]
#   }

#   if (!identical(smooth,0)){
#     values[values==0] <- smooth
#   }
  
#   switch(normalize,
#          "none"={
#          },
#          "first"={
#            vlev  <- length(lev[[1]])
#            palev <- sapply(lev[-1],length)
#            if(length(palev)>0){
#              for (i in 0:(prod(palev)-1)){
#                idx <- i*vlev + 1:vlev
#                vv  <- values[idx]
#                svv <- sum(vv)
#                if (svv!=0)
#                  values[idx] <- vv/svv               
#              }
#            } else {
#              svalues <- sum(values)
#              if (svalues!=0)
#                values <- values / svalues
#            }
#          },
#          "all"={
#            svalues <- sum(values)
#            if (svalues!=0)
#              values <- values / svalues
#          })
  
#   v   <- list(names  = nam, levels=lev, values=values,
#               tab    = NULL,
#               levn   = levn,
#               ncells = prod(levn)
#               ) 
#   class(v) <- "ctab";
#   return(v)
# }












# setGeneric("ctabnames", function(object, ...) standardGeneric("ctabnames"))

# setMethod("ctabnames", signature(object = "ctab"),
#           function(object, ...) {
#             object$names #.vertices(object)
#           })












