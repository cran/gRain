##
## Some set operations
##


#subsetofList <- function(x,ylist){  ## Uses DEDs subsetof - faster than mine
#  any(sapply(ylist, function(y) subsetof(x,y)))
#}

# subsetof <- function (g1, g2) 
#   all(imatch(g1, g2,0)>0)

# imatch <- function (x, table, nomatch = NA_integer_, incomparables = FALSE) {
#     .Internal(match( x, table, 0))
# }



# cardOrder <- function(xlist){
#   x <- xlist
#   len <- unlist(lapply(x,length))
#   unlen <- sort(unique(len))
#   x2 <- NULL
#   for (i in seq(along=unlen)){
#     x2  <- c(x2, x[len==unlen[i]])
#   }
#   x2
# }

# maximalSet <- function(setlist){

#   if (!length(setlist))
#     return(setlist)
#   b     <- setlist
#   b     <- rev(cardOrder(b))
#   bnew  <- list()

#   bnew.i   <- 1
#   bnew     <- as.list(b[1])
#   b[1]     <- NULL

#   if (length(b)>0){
#     for (i in 1:length(b)){
#       if (!subsetofList(b[[i]], bnew)){
#         bnew <- c(bnew, b[i])
#       }
#     }
#   }
#   bnew <- bnew[!is.na(bnew)]
#   bnew <- bnew[!sapply(bnew, is.null)]

#   return(bnew)
# }



