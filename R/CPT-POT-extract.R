extractCPT <- function(x, graph, smooth=0){
  if (!inherits(x, c("data.frame","table", "xtabs")))
    stop("'x' must be one of dataframe, table, xtabs")
  if (!inherits(graph, "graphNEL"))
    stop("'graph' must be a graphNEL object")
  if (!is.DAG(graph))
    stop("'graph' must be a DAG")

  V   <- nodes(graph)
  vpa <- vpar(graph)[V]

  ##.vpa <<- vpa

  if (class(x)[1]=="data.frame"){
      cat("extractCPT - data.frame\n")
      ans <- lapply(vpa, function(ss){
          ##cat(sprintf("---- %s ----\n", toString( ss )))
          zzz <- xtabs(~., data=x[, ss, drop=FALSE])
          ##print(zzz)
          zzz
      })
  } else {
    ans <- lapply(vpa, function(ss){tableMargin(x, ss)})
  }

  #.ans <<- ans
  ans <- lapply(ans, as.parray, normalize="first", smooth=smooth)
  chk <- unlist(lapply(ans, function(zz) any(is.na(zz))))
  nnn <- names(chk)[which(chk)]
  if (length(nnn)>0){
      cat(sprintf("NA's found in conditional probability table(s) for nodes: %s\n", toString(nnn)))
      cat(sprintf("  ... consider using the 'smooth' argument\n"))
  }

  ans
}

extractPOT <- function(x, graph, smooth=0){
  if (!inherits(x, c("data.frame","table")))
    stop("'x' must be a dataframe or a table")
  if (!inherits(graph, "graphNEL"))
    stop("'graph' must be a graphNEL object")
  if (!is.TUG(graph))
    stop("'graph' must be a triangulated undirected graph")

  .rip  <- rip( graph )

  if (class(x)[1]=="data.frame"){
    ans <- .extractPOT_dataframe(x, .rip$cliques, .rip$sep, smooth=smooth)
  } else {
    ans <- .extractPOT_table(x, .rip$cliques, .rip$sep, smooth=smooth)
  }

  attr(ans, "rip")     <- .rip

  dg 	  <- .ug2dag(graph)
  cptlist <- extractCPT(x, dg, smooth=smooth)
  attr(ans, "dag")     <- dg        ## Needed to save network in Hugin format
  attr(ans, "cptlist") <- cptlist   ## Needed to save network in Hugin format

  ans
}

.extractPOT_table <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq    <- cliq[[ii]]
    sp    <- seps[[ii]]
    t.cq  <- tableMargin(x, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp      <- tableMargin(t.cq, sp)
      ans[[ii]] <- tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}

.extractPOT_dataframe <- function(x, cliq, seps=NULL, smooth=0){
  ans <- vector("list", length(cliq))
  for (ii in seq_along(cliq)){
    cq   <- cliq[[ii]]
    sp   <- seps[[ii]]
    xxx  <- xtabs(~., data=x[ , cq, drop=FALSE])
    #xxx  <- as.table(ftable(x[ , cq, drop=FALSE]))
    t.cq <- tableMargin(xxx, cq) + smooth
    names(dimnames(t.cq)) <- cq
    if (!is.null(seps) && length(sp)>0){
      t.sp      <- tableMargin(t.cq, sp)
      ans[[ii]] <- tableOp2(t.cq, t.sp, op=`/`)
    } else {
      ans[[ii]] <- t.cq / sum(t.cq)
    }
  }
  ans
}