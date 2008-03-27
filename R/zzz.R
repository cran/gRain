
.onLoad <- function(lib, pkg)
{
  library.dynam("gRain", package = pkg, lib.loc = lib)
  return(invisible(0))
}
