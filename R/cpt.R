
cpt <- function(v, pa=NULL, values=NULL, gmData=NULL, 
                normalize=TRUE,
                smooth=0, 
                levels=NULL
                ){

  if (is.null(gmData) & is.null(levels)){
    stop("Either gmData or levels must be given")
  }
  if (normalize)
    norm <- 'first'
  else
    norm <- 'none'
  
  vpa <- c(formula2character(v),formula2character(pa))
  
  if (!is.null(gmData)){
    
    uuu <- match(vpa, varNames(gmData))
    if (any(is.na(uuu)))
      stop("Nodes {",paste(vpa[is.na(uuu)],collapse=','), "} do not exist in gmData\n")
    levels <- valueLabels(gmData)[vpa]
    ans    <- ctab (vpa, levels, values, normalize=norm, smooth=smooth)  
  } else {
    ans <- list(vpa=vpa, levels=levels, values=values, normalize=norm, smooth=smooth)
    class(ans)<-"cptTemplate"
  }  
  return(ans)  
}

print.cptTemplate <- function(x, ...){
  cat("v        :", x$vpa[1], "\n")
  if (length(x$vpa[-1]))
    cat("pa       :", x$vpa[-1], "\n")
  cat("levels(v):", x$levels, "\n")
  cat("values   :", x$values, "\n")
}



cptspec <- function(x){

  xclass <- unique(sapply(x,class))
  if (length(xclass)>1){
    stop("Items in x must be of same class...\n")
  }
  switch(xclass,
         "ctab"={
           ##cat("Using ctab...\n")
           vn <- sapply(lapply(x, varNames),"[[", 1)
           vl <- lapply(lapply(x, "[[", "levels"), "[[", 1)           
         },
         "cptTemplate"={
           ## cat("Using cptTemplate...\n")
           vn <- sapply(lapply(x, "[[", "vpa"),"[[",1)
           vl <- lapply(x, "[[", "levels")
           names(vl)<-vn
           plist <- lapply(x, function(xx){
             lev <- vl[xx$vpa] ##  valueLabels(gmd)[xx$vpa]
             ans <- ctab (xx$vpa, lev, xx$values, normalize=xx$normalize,
                          smooth=xx$smooth)  
             ans
           })
           x <- plist
           
         })
  attributes(x) <- list(nodes=vn, levels=vl)
  names(x) <- vn
  class(x) <- "cptspec"
  x
}

as.gmData.cptspec <- function(from){
  newgmData(attr(from,"nodes"), valueLabels=attr(from,"levels"))
}



print.cptspec <- function(x,...){
  cat("cptspec with probabilities:\n")
  lapply(x,
         function(xx){
           vn <- varNames(xx)
           if (length(vn)>1){
             cat(paste(" P(",vn[1],"|",paste(vn[-1],collapse=' '),")\n"))
           } else {
             cat(paste(" P(",vn,")\n"))
           }
         }
         )
}



cpt2 <- function(v, pa=NULL, values=NULL, gmData, smooth=0,
                 normalize=c("none","first","all")){

  normalize <- match.arg(normalize, choices=c("none","first","all"))
  ## Check that (v,pa) is in gmData
  vpa <- c(v,pa)
  uuu <- match(vpa, varNames(gmData))
  if (any(is.na(uuu)))
    stop("Nodes {",paste(vpa[is.na(uuu)],collapse=','), "} do not exist in gmData\n")
  lev <- valueLabels(gmData)[c(v,pa)]
  
  if (is.null(values)){
    d <- observations(gmData)
    if (!is.null(d)){
      if (inherits(d,"cumcounts")){
        dataClass <- "cumcounts"
      } else {
        if (inherits(d,"table")){
          dataClass <- "table"
          class(d) <- "table"  ### FRAGILE
        } else {
          if (inherits(d,"data.frame")){
            dataClass <- "data.frame"
          } else {
            dataClass <- NULL
          }
        }
      }
      switch(dataClass,
             "table"=,"cumcounts"={            
               form   <- as.formula(paste("Freq~",paste(vpa, collapse='+')))
               values <- as.numeric(xtabs(form, data=d))
             },
             "data.frame"={
               cl   <- lapply(d[,vpa], class)
               idxb <- sapply(cl, function(x) "factor" %in% x)
               if (all(idxb)){
                 form   <- as.formula(paste("~",paste(vpa, collapse='+')))
                 values <- as.numeric(xtabs(form, data=d))
               } else {
                 values <- 1
               }
             },
             {stop("Data must be either a table or a dataframe")}
             )
    } else {
      values <- 1
    }
  }
  ctab (c(v,pa), lev, values, normalize=normalize, smooth=smooth)
}
