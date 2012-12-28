random_cpt <- function(dg){
  vpa <- vpar(dg)
  yn   <- c("yes","no")
  nst  <- length(yn)
  cpt <- lapply(vpa, function(zz)
                cptable(zz, values=runif(nst^length(zz)), levels=yn))
  cpt
}
