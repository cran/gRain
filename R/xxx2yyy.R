
dag2cptspec <- function(dag, gmData, smooth=0){
  vpavlist <- vpav(dag)
  ##ans      <- lapply(vpavlist, cpt2, gmData=gmData, smooth=smooth, normalize=TRUE)
  ##cat("dag2cptspec:", smooth, "\n")
  ans      <- lapply(vpavlist, cpt2, gmData=gmData, smooth=smooth, normalize='first')

  cptspec(ans)
}

ug2potspec <- function(ug, gmData, rip, smooth=0){
  cl    <- rip$cliques
  se    <- rip$separators

  ans <- as.list(rep(NA, length(cl)))
  for (i in 1:length(cl)){  
    currc <- cl[[i]]
    currs <- se[[i]]

    #cat("currc:", paste(currc, sep=''),"\n")
    #cat("currs:", paste(currs, sep=''),"\n")
        
    currtab <- cpt2(currc, gmData=gmData, smooth=smooth, normalize='none')

    if (!all(is.na(currs))){
      mtab <- ctabmarg(currtab, currs)
      currtab <- ctabop(currtab, mtab, "/")
    } else {
      ##currtab$values <- currtab$values/sum(currtab$values)
      currtab <- currtab/sum(currtab) ## BRIS
    }
    ans[[i]] <- currtab
   }  

  ##print("ug2potspec - DONE")
  ##ans   <- lapply(cl, cpt2, gmData=gmData, smooth=0, normalize=TRUE)
  #ans   <- lapply(cl, cpt2, gmData=gmData, smooth=0, normalize='none')

  ##print(ans)
  return(ans)
}



formula2character <- function(x){
  if (class(x)=="formula"){
    x2 <- deparse(x)
    x2 <- unlist(strsplit(x2,"[~\\+]"))
    x2 <- gsub(" +","",x2)
    x2 <- x2[as.logical(nchar(x2))]
  } else {
    x
  }
}

ug2dag <- function(ug){
  m     <- mcs(ug)
  if (is.null(m))
    return(NULL)
  
  
  dired <- ed  <- edges(ug)
  if (!is.null(ed)){
    for (i in 1:length(ed)){
      cedge <- ed[[i]]
      dired[[i]] <- m[rev(sort(match(cedge,m)))]
    }
    dired <- c(m[1], dired)
    dag<-newdagsh(dired)
  } else {
    dag <- newdagsh(as.list(nodes(ug)))
  }
  
  return(dag)
}





## If y is not NULL then x and y must be disjoint
##
names2pairs <- function(x, y=NULL, sort=TRUE){
  lenx  <- length(x)
  leny  <- length(y)
  if (length(y)){
    val   <- as.list(rep(NA, lenx*leny))
    k <- 1
    for (i in 1:lenx){
      for (j in 1:leny){
        val[[k]] <- c(x[i], y[j])
        k <- k+1
      }
    }  
  } else {
    if (length(x)==1)
      return(list(x))
    val   <- as.list(rep(NA, lenx*(lenx-1)/2))
    k <- 1
    for (i in 1:(lenx-1)){
      for (j in (i+1):lenx){
        val[[k]] <- c(x[i], x[j])
        k <- k+1
      }
    }  
  }
  if (sort)
    val <- lapply(val, sort)
  val 
}




