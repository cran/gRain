junctionTree <- function(ug, method="mcwh",
                         vn=nodes(ug),
                         nLevels=rep(2,length(vn)),
                         control=list()){
  method <- match.arg(method,c("mcwh","robust"))
  switch(method,
         "mcwh"={
           junctionTree2(ug, vn, nLevels, control)
         },
         "robust"={
           junctionTree1(ug, vn, nLevels, control)
         }
         )
}

junctionTree1 <- function(ug, vn=nodes(ug), nLevels=rep(2,length(vn)), control=list()){
  cat("Blue\n")
  tmdag      <- triangulate(ug,root=NULL)
  val        <- ripOrder(tmdag,root=NULL,nLevels=nLevels)
  val$tug    <- tmdag
  return(val)
}

junctionTree2 <- function(ug, vn=nodes(ug), nLevels=rep(2,length(vn)), control=list()){
  val         <- ripOrderGreen(ug,vn,nLevels,control)
  val$nLevels <- nLevels
  val$tug     <- newugsh(val$cliques)
  return(val)
}

