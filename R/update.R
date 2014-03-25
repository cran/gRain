
"update.CPTgrain" <- function(object,  ...){

    if(!(object$isCompiled))
        object <- compile( object )

    ##cl <- match.call(expand.dots=TRUE)
    args <- list(...)
    arg.names <- names(args)

    if ("CPTlist" %in% arg.names){
        object$cptlist[names(args$CPTlist)] <- args$CPTlist
        pot.with.1          <- .createPotList( object$rip, object$universe )
        newCQpot            <- .insertCpt(object$cptlist, pot.with.1, details=0)
        object$origCQpot    <- newCQpot
        object$tempCQpot    <- newCQpot
        object$equilCQpot   <- .insertNA(pot.with.1)
        object$isPropagated <- FALSE
    }
    object
}























    ## if ("origCQpot" %in% arg.names){
    ##     if (object$isCompiled){
    ##         object$origCQpot    <- args$origCQpot
    ##         object$tempCQpot    <- args$origCQpot
    ##         object$equilCQpot   <- .insertNA(object$equilCQpot)
    ##         object$cptlist      <- .insertNA(object$cptlist)
    ##         object$isPropagated <- FALSE
    ##     }
    ## }
    ## FIXME: careful if object is not compiled


## "update.cpt-grain" <- function(object,  ...){
##   cl <- match.call(expand.dots=TRUE)

##   args <- list(...)
##   arg.names <- names(args)
##   if ("cptlist" %in% arg.names){
##     object$cptlist <- args$cptlist
##     if (object$isCompiled){
##       pot.with.1          <- .createPotList( object$rip, object$universe )
##       newCQpot            <- .insertCpt(args$cptlist, pot.with.1, details=0)
##       object$origCQpot    <- newCQpot
##       object$tempCQpot    <- newCQpot
##       object$equilCQpot   <- .insertNA(pot.with.1)
##       object$isPropagated <- FALSE
##     }
##   }

##   if ("origCQpot" %in% arg.names){
##     if (object$isCompiled){
##       object$origCQpot    <- args$origCQpot
##       object$tempCQpot    <- args$origCQpot
##       object$equilCQpot   <- .insertNA(object$equilCQpot)
##       object$cptlist      <- .insertNA(object$cptlist)
##       object$isPropagated <- FALSE
##     }
##   }
##   ## FIXME: careful if object is not compiled
##   object
## }
