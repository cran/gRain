getgrain <- function(object, name=c("universe", "data", "dag", "cptlist",
                                 "isInitialized", "isCompiled", "isPropagated",
                                 "finding", "control", "details")){

    switch(name,
           universe = object$universe,
           data = object$data,
           dag = object$dag,
           cptlist = object$cptlist,
           isInitialised = object$isInitialized,
           isCompiled = object$isCompiled,
           isPropagated = object$isPropagated,
           finding = object$finding,
           control = object$control,
           details = object$details
           )

}
