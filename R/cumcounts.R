

as.cumcounts <- function(x, Freq="Freq")
  UseMethod("as.cumcounts")
  
as.cumcounts.data.frame <- function(x, Freq="Freq"){
  class(x)<- c("cumcounts", class(x))
  attr(x,"Freq")<-Freq
  return(x)
}

print.cumcounts <- function(x,...){
  print.data.frame(x)
  cat("cumcounts\n")
  return(x)
}

is.cumcounts <- function(x) inherits(x,"cumcounts")


