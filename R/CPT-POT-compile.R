
.parseCPTlist <- function(xi){ ## Create intermediate form of CPTs
  if (is.array(xi))
    cls <- "array"
  else
    cls <- class(xi)[1]

  vpar <- varNames(xi)
  vlev <- valueLabels(xi)[[1]]

  switch(cls,
         "cptable"={
             tmp     <- list(vnam=vpar[1], vlev=vlev, vpar=vpar, values=xi$values,
                             ##normalize="first", smooth=0)
                             normalize=if (xi$normalize) "first" else "none", smooth=xi$smooth)
         },
         "parray"={
             tmp     <- list(vnam=vpar[1], vlev=vlev, vpar=vpar, values=as.numeric(xi),
                             normalize="first", smooth=0)
         },
         "array"={
             tmp     <- list(vnam=vpar[1], vlev=vlev, vpar=vpar, values=as.numeric(xi),
                             normalize="first", smooth=0)
         })
  tmp
}

compileCPT <- function(x, forceCheck=TRUE, details=0){

    #details=1
    zz <- lapply(x, .parseCPTlist)

    vnamList <- lapply(zz, "[[", "vnam")
    vparList <- lapply(zz, "[[", "vpar")
    vlevList <- lapply(zz, "[[", "vlev")
    vn       <- unlist(vnamList)
    names(vlevList) <- vn

    ans       <- vector("list", length(vn))
    names(ans)<- vn
    di        <- unlist( lapply(vlevList, length) )

    if (details>=1) cat(". creating probability tables ...\n")

    for (ii in 1:length(vnamList)){
        if (!forceCheck && class(zz[[ii]])[1]=="parray"){
            ans[[ii]] <- zz[[ii]]
        } else {
            vpar  <- zz[[ ii ]]$vpar
            lev   <- vlevList[vpar]
            val   <- zz[[ii]]$values

            if (forceCheck){
                mm    <- match(vpar, vn)
                if (any(is.na(mm))){
                    sss <- sprintf("compileCPT: Distribution not specified for node(s)\n %s \n",
                                   toString(vpar[which(is.na(mm))]))
                    stop(sss, call.=FALSE)
                }
                if ( prod( unlist(lapply(lev, length))) != length(val) ){
                    cat(sprintf("Error for v,pa(v): %s\n", toString(vpar)))
                    str(lev)
                    str(val)
                    stop("Table dimensions do not match!")
                }
            }

            ans[[ii]] <-
                parray(varNames=vpar,
                       levels    = lev,
                       values    = val,
                       normalize = zz[[ii]]$normalize,
                       smooth    = zz[[ii]]$smooth
                       )
        }
    }

    if (details>=1) cat(". creating dag and checking for acyclicity...\n")

    ## Check for acyclicity
    dg  <- dagList(vparList)
    dgM <- as(dg, "Matrix")
    oo  <- topoSort(dgM)
    if (oo[1]==-1)
        stop("Graph defined by the cpt's is not acyclical...\n");

    universe        <- list(nodes = vn, levels = vlevList, nlev = di)
    attributes(ans) <- list(universe = universe,
                            dag      = dg,
                            vparList = vparList,
                            names    = vn )

    class(ans) <- "CPTspec"
    ans
}














compilePOT <- function(x){
  uug <- ugList(lapply(x, function(a) names(dimnames(a))))
  if (!is.TUG(uug))
    stop("Graph defined by potentals is not triangulated...\n")

  lll       <- unlist(lapply(x, dimnames),recursive=FALSE)
  nnn       <- names(lll)
  iii       <- match(unique(nnn), nnn)
  levels    <- lll[iii]
  vn        <- nnn[iii]
  di        <- c(lapply(levels, length), recursive=TRUE)
  names(di) <- vn
  universe  <- list(nodes = vn, levels = levels, nlev   = di)
  ans       <- x
  attributes(ans) <- list(universe = universe,
                          ug       = uug,
                          rip      = attr(x, "rip"),
                          dag      = attr(x, "dag"),      ## Needed to save network in Hugin format
                          cptlist  = attr(x, "cptlist"))  ## Needed to save network in Hugin format

  class(ans) <- "POTspec"
  return(ans)
}

print.CPTspec <- function(x,...){
  cat("CPTspec with probabilities:\n")
  lapply(x,
         function(xx){
           vn <- varNames(xx)
           if (length(vn)>1){
             cat(paste(" P(",vn[1],"|",paste(vn[-1],collapse=' '),")\n"))
           } else {
             cat(paste(" P(",vn,")\n"))
           }
         })
  return(invisible(x))
}

summary.CPTspec <- function(object,...){
  print(object)
  cat(sprintf("attributes:\n %s\n", toString(names(attributes(object)))))
  cat(sprintf("names:\n %s \n", toString(attributes(object)$names)))
  cat(sprintf("nodes:\n %s \n", toString(attributes(object)$nodes)))
  cat(sprintf("levels: \n"))
  str(attributes(object)$levels)
  cat(sprintf("vparList: \n"))
  str(attributes(object)$vparList)
  cat(sprintf("nlev: \n"))
  str(attributes(object)$nlev)
  cat(sprintf("dag: \n"))
  print(attributes(object)$dag)
  cat(sprintf("dagM: \n"))
  print(attributes(object)$dagM)
  return(invisible(object))
}

print.POTspec <- function(x,...){
  cat("POTspec with potentials:\n")
  lapply(x,
         function(xx){
           vn <- names(dimnames(xx))
           cat(   "(", paste(vn, collapse=' '),") \n")
         })

  return(invisible(x))
}



## .parse.cpt2 <- function(xi){
##   if (is.array(xi))
##     cls <- "array"
##   else
##     cls <- class(xi)[1]
##   switch(cls,
##          "parray"={
##            vpar    <- varNames(xi)
##            vn      <- vpar[1]
##            vlev    <- valueLabels(xi)[[1]]
##            values  <- as.numeric(xi)
##            tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values,
##                            normalize="first", smooth=0)
##          },
##          "cptable"={

##            vpar    <- varNames(xi)
##            vn      <- vpar[1]
##            vlev    <- valueLabels(xi)[[1]]

##            tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=xi$values,
##                            normalize=if (xi$normalize) "first" else "none", smooth=xi$smooth)
##          },
##          "array"={
##            vpar    <- names(dimnames(xi))
##            vn      <- vpar[1]
##            vlev    <- dimnames(xi)[[1]]
##            values  <- as.numeric(xi)
##            tmp     <- list(vnam=vn, vlev=vlev, vpar=vpar, values=values,
##                            normalize="first", smooth=0)
##          })
##   return(tmp)
## }





    ##     }

    ##     if (class(zz[[ii]])[1]=="parray" && !forceCheck){
    ##         ans[[ii]] <- zz[[ii]]
    ##     } else if
    ##         if (forceCheck){
    ##             vpar  <- zz[[ ii ]]$vpar
    ##             lev   <- vlevList[vpar]
    ##             val   <- zz[[ii]]$values
    ##             mm    <- match(vpar, vn)
    ##             if (any(is.na(mm))){
    ##                 sss <- sprintf("compileCPT: Distribution not specified for node(s)\n %s \n",
    ##                                toString(vpar[which(is.na(mm))]))
    ##                 stop(sss, call.=FALSE)
    ##             }
    ##             if ( prod( unlist(lapply(lev, length))) != length(val) ){
    ##                 cat(sprintf("Error for v,pa(v): %s\n", toString(vpar)))
    ##                 str(lev)
    ##                 str(val)
    ##                 stop("Table dimensions do not match!")
    ##             }
    ##         }
    ##     } else {
    ##         vpar  <- zz[[ ii ]]$vpar
    ##         lev   <- vlevList[vpar]
    ##         val   <- zz[[ii]]$values
    ##         if (forceCheck){
    ##             mm    <- match(vpar, vn)
    ##             if (any(is.na(mm))){
    ##                 sss <- sprintf("compileCPT: Distribution not specified for node(s)\n %s \n",
    ##                                toString(vpar[which(is.na(mm))]))
    ##                 stop(sss, call.=FALSE)
    ##             }
    ##             if ( prod( unlist(lapply(lev, length))) != length(val) ){
    ##                 cat(sprintf("Error for v,pa(v): %s\n", toString(vpar)))
    ##                 str(lev)
    ##                 str(val)
    ##                 stop("Table dimensions do not match!")
    ##             }
    ##         }

    ##         ans[[ii]] <-
    ##             parray(vpar,
    ##                    values    = val,
    ##                    normalize = zz[[ii]]$normalize,
    ##                    smooth    = zz[[ii]]$smooth,
    ##                    levels    = lev)
    ##     }

    ## }

    ## for (ii in 1:length(vnamList)){
    ##     vpar  <- zz[[ ii ]]$vpar
    ##     lev   <- vlevList[vpar]
    ##     val   <- zz[[ii]]$values
    ##     if (forceCheck){
    ##         mm   <- match(vpar, vn)
    ##         if (any(is.na(mm))){
    ##             sss <- sprintf("compileCPT: Distribution not specified for node(s)\n %s \n",
    ##                            toString(vpar[which(is.na(mm))]))
    ##             stop(sss, call.=FALSE)
    ##         }

    ##         if ( prod( unlist(lapply(lev, length))) != length(val) ){
    ##             cat(sprintf("Error for v,pa(v): %s\n", toString(vpar)))
    ##             str(lev);  str(val); stop("Table dimensions do not match!")
    ##         }
    ##     }

    ##     print(class(x[[ii]]))
    ##     ans[[ii]] <-
    ##         parray(vpar,
    ##                values    = val,
    ##                normalize = zz[[ii]]$normalize,
    ##                smooth    = zz[[ii]]$smooth,
    ##                levels    = lev)
    ## }


        ##if (details>=2 && (ii %% 1000 == 0))
        ##cat(sprintf(".. ii = %6i, v,pa(v): %s\n", ii, toString(vpar)))


    ##vn        <- c(vnamList, recursive=TRUE)
    #di        <- c(lapply(vlevList, length), recursive=TRUE)
    #names(di) <- vn

    ##chk<<-list(vnamList=vnamList, vparList=vparList, vlevList=vlevList, vn=vn)

