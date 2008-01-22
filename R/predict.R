predict.compgmInstance <- function(object, response, predictors,
                                   newdata, type="class", ...){

   
  type <- match.arg(type, c("class","distribution"))
  nstate <- nodeStates(object,response)
  if (missing(predictors))
    predictors  <- setdiff(names(newdata),response)
  
  p.evec <- rep(NA, nrow(newdata))
  ans <- lapply(nstate, function(a){
    v <- matrix(NA, nc=length(a),nr=nrow(newdata))
    colnames(v) <- a
    v})
  
  for (i in 1:nrow(newdata)){
    case      <- newdata[i,predictors]
    objecttmp1    <- enterEvidence(object,nodes=names(case), states=case)
    p.e       <- pevidence(objecttmp1)
    p.evec[i] <- p.e
    for (j in 1:length(response)){
      pj   <- nodeMarginal(objecttmp1, response[j])[[1]]$values
      ans[[j]][i,] <- pj
    }
  } 

  if (type=="class"){
    ns <- nodeStates(object, response)
    for (i in 1:length(ans)){
      a<-ans[[i]]
      mlc <- apply(a,1,which.max)
      ans[[i]] <- ns[[i]][mlc]
    }
  }
  
  value <- list(pred=ans, pevidence=p.evec)
  return(value)
}
