
.onLoad <- function(lib, pkg)
{
  library.dynam("gRain", package = pkg, lib.loc = lib)
  
#   if((R.version$major == 1) && (as.numeric(R.version$minor) < 9))
#     packageDescription <- package.description
  
#   cat("\n")
#   cat("-------------------------------------------------------------\n")
#   cat(packageDescription("gRain", lib = lib, field="Title"))
#   cat("\n")
#   ver  <- packageDescription("gRain", lib = lib, field="Version")
#   maint<- packageDescription("gRain", lib = lib, field="Maintainer")
#   autho<- packageDescription("gRain", lib = lib, field="Author")
#   descr<- packageDescription("gRain", lib = lib, field="Description")
#   built<- packageDescription("gRain", lib = lib, field="Built")
#   URL  <- packageDescription("gRain", lib = lib, field="URL")
#   cat(descr,"\n")
#   cat(paste("gRain, version", ver,  "is now loaded\n"))
#   cat("Authors:",autho,"\n")
#   cat("Maintained by",maint,"\n")
#   cat("Webpage:",URL,"\n")
#   cat("\nBuilt:",built,"\n")
#   cat("-------------------------------------------------------------\n")

#  require(methods)
#  require(MASS)
  return(invisible(0))
}
