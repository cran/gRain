.timing <- function(text, control, t0){
  if (!is.null(control$timing) && control$timing)
    cat(sprintf("%40s", text), proc.time()-t0,"\n")

}

compile.grain <-
  function(object, propagate=FALSE, root=NULL, 
           control=object$control, details=0,...) {
    #method <- match.arg(tolower(method), c("mcwh","r"))
    NextMethod("compile") 
}

compile.CPTgrain <- 
  function(object, propagate=FALSE, root=NULL, 
           control=object$control, details=0,...){
    ## object has slots:
    ## [1] "universe"      "data"          "dag"           "cptlist"       "isInitialized"
    ## [6] "isCompiled"    "isPropagated"  "finding"       "control"       "details"
     
    t0 <- t00 <-  proc.time()
    mdagM <- moralizeMAT(as(object$dag,"Matrix"))
    .timing(" Time: moralize:", control, t0)
    vn    <- colnames(mdagM)
    nlev  <- object$universe$nlev[vn]

    if (!is.null(root) && length(root)>1){      
      t0 <- proc.time()
      dn <- dimnames(mdagM)    
      ft <- names2pairs(match(root, vn),sort=FALSE, result="matrix")
      ft <- rbind(ft,ft[,2:1,drop=FALSE])      
      mdagM <- sp_setXtf1(mdagM, ft)
      dimnames(mdagM) <- dn
      .timing(" Time: setting root:", control, t0)
    }

    t0    <- proc.time()
    ugM   <- triangulateMAT(mdagM)
    .timing(" Time: triangulate:", control, t0); t0 <- proc.time()
    
    .rip   <- ripMAT( ugM )
    .timing(" Time: rip:", control, t0); t0 <- proc.time()

    jt    <- .createJTreeGraph(.rip)
    .timing(" Time: create junction tree:", control, t0); t0 <- proc.time()
    
### Insert potentials; ## Input: rip, universe, cptlist
    pot.with.1   <- .createPotList( .rip, object$universe )     
    origCQpot    <- tempCQpot <- .insertCpt(object$cptlist, pot.with.1, details)
    equilCQpot   <- .insertNA(pot.with.1)
    .timing(" Time: insert cpt into potentials:", control, t0)

    ug    <- as(ugM,   "graphNEL")
    mdag  <- as(mdagM, "graphNEL")
       
### Collect results
    ans      <- list(rip         = .rip,
                     jt          = jt,
                     ug          = ug,
                     equilCQpot  = equilCQpot,
                     tempCQpot   = tempCQpot,
                     origCQpot   = origCQpot, 
                     mdag        = mdag,
                     details     = details )    
    ans        <- c(object, ans)
    class(ans) <- class(object)
    
    ans$isCompiled <- TRUE
    ans$control    <- control
    .timing(" Time: (total) compile:", control, t00)    

    if (propagate){     ## Propagate if asked to
      .infoPrint(details, 1, cat (".Initializing network\n"))
      ans <- propagate(ans)
    }  
    return(ans)
  }

## NOTICE: the compiled object will contain a dag and a cptlist.
## These are not used for any calculations; only used for saving
## the network in Hugin format...

compile.POTgrain <-
  function(object, propagate=FALSE, root=NULL, 
           control=object$control, details=0,...) {

    ## object has slots
    ## [1] "universe"      "data"          "equilCQpot"    "ug"            "rip"          
    ## [6] "dag"           "cptlist"       "isInitialized" "isCompiled"    "isPropagated" 
    ## [11] "finding"       "control"       "details"         
    t00 <-  proc.time()
    jt  <- .createJTreeGraph(object$rip)    
    ans     <- list(jt          = jt,
                    tempCQpot   = object$equilCQpot,
                    origCQpot   = object$equilCQpot, 
                    mdag        = object$ug,
                    details     = details )
    
    object$equilCQpot   <- .insertNA(object$equilCQpot)   
    #object$details <- NULL
    ans            <- c(object, ans)
    class(ans)     <- class(object)

    ans$control    <- control    
    ans$isCompiled <- TRUE
    .timing(" Time: (total) compile:", control, t00)    
    
    if (propagate){     ## Propagate if asked to
      .infoPrint(details, 1, cat(".Initializing network\n"))
      ans             <- propagate(ans)
    }  
    return(ans)
  }




.createJTreeGraph <- function(rip){
  if (length(rip$cliques)>1){
    ft <-cbind(rip$parents, 1:length(rip$parents))
    ft <- ft[ft[,1]!=0,, drop=FALSE]
    V <- seq_along(rip$parents)
    if (nrow(ft)==0){
      jt <- new("graphNEL", nodes = as.character(V), edgemode = "undirected")
    } else {
      jt <- ftM2graphNEL(ft, V=as.character(V), edgemode="undirected")
    }
  } else {
    jt <- new("graphNEL", nodes = "1", edgemode = "undirected")
  }
  return(jt)
}



