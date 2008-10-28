##
## Reading / writing Bayesian networks from / to HUGIN net files
##


printlist <- function(x,d=0) UseMethod("printlist")

##printlist.numeric <- function(x,d=0){
printlist.default <- function(x,d=0){
  paste("(", paste(x,collapse=' '),")",sep='')
}
printlist.list <- function(x,d=0){
  tmp     <- unlist(lapply(x, printlist, d+2),recursive=FALSE)
  prefix  <- as.list(c("(",rep(" ",length(tmp)-1)))
  posfix  <- as.list(c(rep(" ",length(tmp)-1),")"))
  as.list(mapply(function(l,x,r) {paste(l,x,r,sep='')}, prefix, tmp, posfix))
}

splitVec <- function(val, lev) UseMethod("splitVec")

splitVec.list <- function(val, lev){
  lapply(val, splitVec, lev)
}

##splitVec.numeric <- function(val, lev){
splitVec.default <- function(val, lev){
  m    <- matrix(val,nc=lev)
  cval <- unlist(apply(m,2,list),recursive=FALSE)
  cval
}




getLine   <- function(con) {
  readLines(con, n=1)
}

hasToken  <- function(token, cline) {
  ##print(cline)
  cline <- gsub("^ +","",cline)
  a <- unlist(strsplit(cline," "))[1]

  if (!is.na(a))
    a==token
  else
    FALSE
}

tokenIdx <- function(token, x){
  idx <- which(as.logical(lapply(x, function(d) grep(token,d))))
  idx
}

readHuginNet <- function(file, trace=0){

  filename <- file
  if (trace>=1)cat(".HUGIN netfile:", filename,"\n")
  nodeCount <- 0
  con <- file(filename, "rb")
  repeat{
    cline <- getLine(con);  #print(cline)
    if (!length(cline))
      break()

    if (hasToken("node", cline)) ## Fragile if 'node' is the name of a variable...
      nodeCount <- nodeCount + 1
  }
  close(con)

  if(trace>=3) cat("...there are around", nodeCount, "nodes \n")

  ## Data structure for holding specification (possibly too long)
  ##
  nodeList <- potentialList <- as.list(rep(NA, nodeCount))
  
  con <- file(filename, "rb")
  currNode <- currPotential <- 1
  state<-"start"
  repeat{
    cline <- getLine(con);  #print(cline)
    if (!length(cline))
      break()
    switch(state,
           "start"={
             if (hasToken("net",cline)){
               state="net";    if(trace>=2)cat("..NET action\n")
               wline <- cline
             }
           },
           "net"={
             wline <- c(wline, cline)
             if (hasToken("}",cline)){
               state="run1";           if(trace>=2)cat("..end NET action\n")
               if(trace>=3){print("...NET"); print (wline)}
             }
           }, 
           "run1"={
             if (hasToken("node", cline)){
               state="node";             if(trace>=2)cat("..NODE action\n")               
             } else {
               if (hasToken("potential", cline)){
                 state="potential";  if(trace>=2) cat("..POTENTIAL action\n")
               }
             }
             wline <- cline
           },
           "node"={
             wline <- c(wline, cline)
             if (hasToken("}",cline)){
               state="run1";           if(trace>=2)cat("..end NODE action\n")
               if(trace>=3) {print("...NODE"); print (wline)}
               nodeList[[currNode]] <- wline;
               currNode <- currNode + 1 
             }     
           },
           "potential"={
             wline <- c(wline, cline)
             if (hasToken("}",cline)){
               state="run1";           if(trace>=2)cat("..end POTENTIAL action\n")
               if(trace>=3){print("...POTENTIAL"); print (wline)}
               potentialList[[currPotential]] <- wline;
               currPotential <- currPotential + 1 
               
             }          
           }  
           )
  }
  close(con)

  nodeList <- nodeList[!sapply(lapply(nodeList, is.na),all)]
  potentialList <- potentialList[!sapply(lapply(potentialList, is.na),all)]


  value <- structure(list(nodeList=nodeList, potentialList=potentialList))
  return(value)
}



capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
                           {s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

toCamel <- function(s){
  s<-gsub(" +"," ",s)
  s<-unlist(strsplit(s, " "))
  paste(sapply(s, capwords),collapse='')
}

toCamel <- function(s){
  s<-gsub(" +"," ",s)
  s<-unlist(strsplit(s, " "))
  paste(c(s[1],sapply(s[-1], capwords)),collapse='')
}



getNodeSpec <- function(nodeSpec){

  tmp <- nodeSpec[tokenIdx("node", nodeSpec)]
  nodeVar <- gsub("node +","",tmp)[1]
  nodeVar <- gsub(" +","",nodeVar)
  
  
  tmp <- nodeSpec[tokenIdx("label", nodeSpec)]
  nodeLabel <- gsub(" +label += +","",tmp); 
  nodeLabel <- gsub(";", "", nodeLabel)
  nodeLabel <- gsub('"',"", nodeLabel)

  nodeLabel <- gsub(" +"," ",nodeLabel)

  if (length(nodeLabel) && nchar(nodeLabel)>0){
    
    nodeLabel <- toCamel(nodeLabel)
    
    nl <- gsub("[^[:alnum:]]","",nodeLabel)
    nodeLabel <- gsub("[^[:alnum:]|\\.]","",nodeLabel)
    
    base<-as.character(0:9)
    if(subsetof(unlist(strsplit(nl,"")), base)){
      nodeLabel <- paste("X",nodeLabel,sep='')
    }    
  } else {
    ##if (nchar(nodeLabel)==0)
    nodeLabel <- nodeVar
  }
    
  tmp <- nodeSpec[tokenIdx("states", nodeSpec)]
  nodeStates <- gsub(" +states += +","",tmp); 
  nodeStates <- gsub("[\\(,\\);]","",nodeStates); 
  nodeStates <- unlist(strsplit(nodeStates, '\\"'))
  nodeStates <- sapply(nodeStates, function(d) gsub("^ +","",d))
  nodeStates <- nodeStates[sapply(nodeStates, nchar)>0]

  nodeStates <- sapply(nodeStates, toCamel)
  
  nodeStates <- gsub(" +",".", nodeStates)
  names(nodeStates)<-NULL

  
  value <- list(nodeVar=nodeVar, nodeLabel=nodeLabel, nodeStates=nodeStates)
  value
}

getPotentialSpec <- function(potSpec){
  tmp <- potSpec[tokenIdx("potential", potSpec)]
  tmp <- gsub("potential +","", tmp)
  tmp <- gsub("[\\(,\\),|]","", tmp)
  tmp <- gsub(" +"," ", tmp)
  tmp <- unlist(strsplit(tmp," "))
  tmp <- tmp[sapply(tmp, nchar)>0]
  
  nodeVar <- tmp[1]
  parentVar <- tmp[-1]
  
  tmp <- potSpec[-(1:(tokenIdx("data", potSpec)-1))]
  pot <- NULL
  for (i in 1:length(tmp)){
    x <- tmp[i]
    x <- gsub(" *data *","", x)
    x <- gsub("\\%.*","", x)
    x <- gsub("\t","",x)
    x <- gsub(" *= *", "",x)
    x <- gsub("[\\(,\\),\\}]","", x)
    x <- gsub(";","",x)
    x <- unlist(strsplit(x," +"))
    x <- as.numeric(x[sapply(x,nchar)>0])
    pot <- c(pot,x)
  }
  
  value <- list(nodeVar=nodeVar, parentVar=rev(parentVar), potential=pot)
  value
}


print.huginNet <- function(x,...){
  cat ("Specfication of BN from HUGIN .net file\n")
  lapply(x$nodeList, function(d) 
         cat(paste(d$nodeVar, d$nodeLabel, paste(d$nodeStates, collapse=' ')),"\n")
         )
  
  lapply(x$potentialList, function(d) 
         cat(paste(d$nodeVar, paste(d$parentVar, collapse=' '),
                   paste(d$potential, collapse=' ')),"\n")
         )
  x
}


as.gmData.huginnet <- function(from){
  
  ccshort   <-sapply(from$nodeList, function(x)x$nodeVar)
  ccnames   <-sapply(from$nodeList, function(x)x$nodeLabel)
  cclabels  <-lapply(from$nodeList, function(x)x$nodeStates)
  names(cclabels) <- ccnames

  gmd <- newgmData(ccnames,valueLabels=cclabels,
                    nLevels= sapply(cclabels,length), shortNames=ccshort)
  return(gmd)
}



loadHuginNet <- function(file, description=rev(unlist(strsplit(file, "/")))[1],
                         trace=0){

  filename <- file
  
  x     <-readHuginNet(filename,trace)
  x     <-transformHuginNet2internal(x)

  gmd   <- as.gmData(x)
  pl   <- lapply(x$potentialList, hpot2cpt, gmd)

  pl <- cptspec(pl)
  value <- list(cptlist=pl, gmData=gmd, description=description)
  class(value) <- "huginNet"
  return(value)
}

print.huginNet <- function(x,...){
  cat("Internal representation of HUGIN net file:", x$description, "\n")
  cat("Class: ", class(x), "\n")
  cat("\nAvailable components:", paste(names(x), sep=' '), "\n")
}

  
hpot2cpt <- function(cpot,gmd){
  #print(cpot)
  v   <- varNames(gmd)[match(cpot$nodeVar,shortNames(gmd))]
  pa  <- varNames(gmd)[match(cpot$parentVar,shortNames(gmd))]
  #cat("v:", v, "\n pa:", paste(pa, collapse=' '),"\n")
  cptable(v=v, pa=pa, values=cpot$potential, gmData=gmd)
}


makeNodeNamesUnique <- function(nodeList2){
  nl<-t(sapply(nodeList2, function(d)unlist(d[1:2])))
  ##pl<-lapply(potentialList2, function(d)unlist(d[1:2]))

  ## cat("makeNodeNamesUnique\n")
##   nl <<- nl
  
  nonunique <- names(which(table(nl[,2])>1))
  
  if (length(nonunique)){
    cat ("Label(s): {", nonunique, "} appears mode than once in NET file\n")
    for (i in 1:length(nonunique)){
      cnu <- nonunique[i]
      idx<-which(cnu ==nl[,2])
      for (j in idx){
        a <- nodeList2[[j]]$nodeVar
        cat("  Replacing label", cnu, " with node name", a, "\n")
        nodeList2[[j]]$nodeLabel <- a 
      }
    }
  }
  
  return(list(nodeList=nodeList2, nonunique=nonunique))
}



transformHuginNet2internal <- function(x){
  nodeList2 <- lapply(x$nodeList, getNodeSpec)
  potentialList2 <- lapply(x$potentialList, getPotentialSpec)

  nl <- makeNodeNamesUnique(nodeList2)

  repeat{
    if (length(nl$nonunique)==0)
      break()
    nl <- makeNodeNamesUnique(nl$nodeList)
  }

  nodeList2 <- nl$nodeList

  
  
  value <- structure(list(nodeList=nodeList2, potentialList=potentialList2))
  class(value)<- "huginnet"

  return(value)
}



saveHuginNet <- function(bn, file, trace=0){
  
  gmd     <- bn$gmData
  cptlist <- bn$cptlist

  vlab <- valueLabels(gmd)
  vnam <- names(vlab)
  nn   <- length(vlab)

  th     <- cumsum(c(0,rep(2*pi/nn, nn-1)))
  r      <- 100
  coords <- lapply(th, function(d) round(r+r*c(cos(d), sin(d))))

  con <- file(file, "wb")

  ## Write (trivial) net specification
  ##
  writeLines("net\n{", con)
  writeLines("  node_size = (100 30);", con)
  writeLines("\n}\n\n", con)
  
  ## Write node specification
  ##
  for (i in 1:length(vlab)){
    st<-paste("node ", vnam[i],"\n","{","\n",sep='')
    writeLines(st,con,sep="")
    ## cat(st)
    st <- paste("   label = \"\";","\n")
    writeLines(st,con,sep="")
    ## cat(st)
    st <- paste("   position = (", paste(coords[[i]], collapse=' '), ");\n")
    writeLines(st,con,sep="")
    ## cat(st)
    
    st2 <- sapply(vlab[[i]], function(d) paste('"',d,'"',sep=''))
    st  <- paste("   states = (", paste(st2, collapse=' '), ");\n")
    writeLines(st,con,sep="")
    ## cat(st)
    st <- paste("}\n")
    writeLines(st,con,sep="")
    ## cat(st)
  }


  for (ii in 1:length(cptlist)){

    cpot <- cptlist[[ii]]
    
    ##nam <- cpot$varNames
    ##lev <- cpot$levels
    ##val <- cpot$values

    nam <- varNames(cpot)    ## BRIS
    lev <- valueLabels(cpot) ## BRIS
    val <- cpot              ## BRIS
    
    v  <- nam[1]
    pa <- nam[-1]
    
    lev   <- rev(lev[-1])
    wval  <- val
    if (length(lev)>0){
      for (i in 1:length(lev)){
        print("splitVec:"); print(wval); print(class(wval))
        wval<-splitVec(wval,length(lev[[i]]))
      }
    }
    ##print(wval); print(class(wval))
    plx <- printlist(wval)
    
    if (length(pa)){
      st <- paste("potential (",v, "|", paste(rev(pa), collapse=' '),")\n")
      writeLines(st,con,sep="")
      ## cat(st)
      st <- "{\n";
      writeLines(st,con,sep="")
      ## cat(st)
      st <- paste("   data = \n")
      writeLines(st,con,sep="")
      ## cat(st)
      ##a<-lapply(plx, cat, "\n")
      a<-lapply(plx, writeLines, con, sep="\n")
      st <- paste(";\n}\n")
      writeLines(st,con,sep="")
      ## cat(st)
      
    } else {
      st <- paste("potential (", v, ")\n")
      writeLines(st,con,sep="")
      ## cat(st)
      st <- "{\n";
      writeLines(st,con,sep="")
      ## cat(st)
      st <- paste("   data = ", plx, ";\n")
      writeLines(st,con,sep="")
      ## cat(st)
      st <- "}\n\n";
      writeLines(st,con,sep="")
      ## cat(st)
    }
  }
 
  close(con)
  

}

