compile <- 
function (object, ...) 
{
    UseMethod("compile")
}

propagate <- 
function (object, ...) 
{
    UseMethod("propagate")
}


nodeNames  <- function(x) UseMethod("nodeNames")

nodeStates <- function(x, nodes=nodeNames(x)) UseMethod("nodeStates")
