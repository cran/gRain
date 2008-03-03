ripOrderGreen <- function(ug, vn=nodes(ug), nLevels=rep(2,length(vn)),control=list()){

  t0    <- proc.time()
  
  amat  <- as.adjmat(ug, vn)
  if (!is.null(control$timing) && control$timing)
    cat(" Time: (detail) Adjancency matrix:", proc.time()-t0,"\n") 

  ans   <- mcwhSH(amat, nLevels, quiet=TRUE)
  
  if (!is.null(control$timing) && control$timing)
    cat(" Time: (detail) mcwh:", proc.time()-t0,"\n")

  ## Turn indices into variable names
  ##
  cq    <- lapply(ans$j.cq, function(a) vn[a])
  sp    <- lapply(ans$j.sp, function(a) vn[a])
  sp[1] <- NA
  
  x   <- do.call("rbind", lapply(ans$j.tree, function(xx)xx[1,]))
  pa  <- c(NA,x[-1,2])
  ch  <- x[,1]

  rip2 <-
    structure(list(nodes      = vn,               
                   cliques    = cq,
                   separators = sp,
                   pa         = pa,
                   ch         = ch),
              class="ripOrder")
  return(rip2)
}

## This is essentially a copy of Peter Greens mcwh function
##

mcwhSH<-function (j.adj,var.nvals=rep(2,ncol(j.adj)),quiet=FALSE) 
{
#   if(!is.loaded('mcwh'))
# 	{
#           lib<-paste('Grappa',.Platform$dynlib.ext,sep='')
#           dyn.load(lib)
#           cat('...',lib,'loaded\n')
# 	}
  
  nn<-apply(j.adj,1,sum)
  nv<-nrow(j.adj)
  cnbr<-NULL
  for(i in 1:nv) cnbr<-c(cnbr,(1:nv)[j.adj[i,]])
  nc<-length(cnbr)

#  cat("BEFORE:\n")
#  cat("j.adj\n"); print(1*j.adj)
#  cat("nn (#nb for each node)\n"); print(nn)
#  cat("nv\n"); print(nv)
#  cat("cnbr (nb's for each node)\n"); print(cnbr)
  
  zz<-.Fortran('mcwh',as.integer(nv),as.integer(nn),as.integer(var.nvals),
               as.integer(nc),as.integer(cnbr),
               it=integer(1),inbre=integer(1),elim=integer(nv),le=integer(nv),
               nne=integer(nv),nbre=integer(nc),
               integer(nv*nv),double(nv),integer(nv),
               PACKAGE="gRain")


  ncq  <- zz$it+1
  iz   <- cumsum(zz$nne)
  ia   <- 1+c(0,iz[-ncq])
  j.cq <- list();
  j.sp <- list()
  j.cq[[1]] <- sort(zz$nbre[ia[ncq]:iz[ncq]])
  w<-rep(0,ncq)
  if(ncq>1) for(i in 2:ncq)
    {
      s<-zz$nbre[ia[ncq+1-i]:iz[ncq+1-i]]
      j.sp[[i]]<-sort(s); j.cq[[i]]<-sort(c(s,zz$le[ncq+1-i]))
      w[i]<-ncq+1-min(zz$elim[s])
    }
#  cat("AFTER:\n")
#  cat("ncq\n"); print(ncq)
#  cat("iz\n"); print(iz)
#  cat("nbre\n"); print(zz$nbre)
  ##cat(w,'\n')

  jt<-list()
  if(ncq>1) for(i in 2:ncq)
    {
      wi<-w[i]
      if(!quiet) cat('cliques',wi,i,'linked by separator',i,'\n')
      if(length(jt)<i)
        jt[[i]]<-matrix(c(i,wi),nrow=1)
      else
        {
          if(is.null(jt[[i]])) jt[[i]]<-matrix(c(i,wi),nrow=1)
          else jt[[i]]<-rbind(jt[[i]],c(i,wi))
        }
      if(length(jt)<wi)
        jt[[wi]]<-matrix(c(i,i),nrow=1)
      else
        {
          if(is.null(jt[[wi]])) jt[[wi]]<-matrix(c(i,i),nrow=1)
          else jt[[wi]]<-rbind(jt[[wi]],c(i,i))
        }
    }
  j.tree<-jt
  
  invisible(list(j.cq=j.cq,j.sp=j.sp,j.tree=j.tree))
}


# mcwhSH<-function (j.adj,var.nvals=rep(2,ncol(j.adj)),quiet=FALSE) 
# {
# #   if(!is.loaded('mcwh'))
# # 	{
# #           lib<-paste('Grappa',.Platform$dynlib.ext,sep='')
# #           dyn.load(lib)
# #           cat('...',lib,'loaded\n')
# # 	}
  
#   nn<-apply(j.adj,1,sum)
#   nv<-nrow(j.adj)
#   cnbr<-NULL
#   for(i in 1:nv) cnbr<-c(cnbr,(1:nv)[j.adj[i,]])
#   nc<-length(cnbr)
  
  
#   zz<-.Fortran('mcwh',as.integer(nv),as.integer(nn),as.integer(var.nvals),
#                as.integer(nc),as.integer(cnbr),
#                it=integer(1),inbre=integer(1),elim=integer(nv),le=integer(nv),
#                nne=integer(nv),nbre=integer(nc),
#                integer(nv*nv),double(nv),integer(nv),
#                PACKAGE="gRain")
  
#   ncq<-zz$it+1
#   iz<-cumsum(zz$nne)
#   ia<-1+c(0,iz[-ncq])
#   j.cq<-list(); j.sp<-list()
#   j.cq[[1]]<-sort(zz$nbre[ia[ncq]:iz[ncq]])
#   w<-rep(0,ncq)
#   if(ncq>1) for(i in 2:ncq)
#     {
#       s<-zz$nbre[ia[ncq+1-i]:iz[ncq+1-i]]
#       j.sp[[i]]<-sort(s); j.cq[[i]]<-sort(c(s,zz$le[ncq+1-i]))
#       w[i]<-ncq+1-min(zz$elim[s])
#     }
#                                         ##cat(w,'\n')

#   jt<-list()
#   if(ncq>1) for(i in 2:ncq)
#     {
#       wi<-w[i]
#       if(!quiet) cat('cliques',wi,i,'linked by separator',i,'\n')
#       if(length(jt)<i)
#         jt[[i]]<-matrix(c(i,wi),nrow=1)
#       else
#         {
#           if(is.null(jt[[i]])) jt[[i]]<-matrix(c(i,wi),nrow=1)
#           else jt[[i]]<-rbind(jt[[i]],c(i,wi))
#         }
#       if(length(jt)<wi)
#         jt[[wi]]<-matrix(c(i,i),nrow=1)
#       else
#         {
#           if(is.null(jt[[wi]])) jt[[wi]]<-matrix(c(i,i),nrow=1)
#           else jt[[wi]]<-rbind(jt[[wi]],c(i,i))
#         }
#     }
#   j.tree<-jt
  
#   invisible(list(j.cq=j.cq,j.sp=j.sp,j.tree=j.tree))
# }

