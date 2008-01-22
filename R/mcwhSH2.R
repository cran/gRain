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
  
  zz<-.Fortran('mcwh',as.integer(nv),as.integer(nn),as.integer(var.nvals),
               as.integer(nc),as.integer(cnbr),
               it=integer(1),inbre=integer(1),elim=integer(nv),le=integer(nv),
               nne=integer(nv),nbre=integer(nc),
               integer(nv*nv),double(nv),integer(nv),
               PACKAGE="gRain")
  
  ncq<-zz$it+1
  iz<-cumsum(zz$nne)
  ia<-1+c(0,iz[-ncq])
  j.cq<-list(); j.sp<-list()
  j.cq[[1]]<-sort(zz$nbre[ia[ncq]:iz[ncq]])
  w<-rep(0,ncq)
  if(ncq>1) for(i in 2:ncq)
    {
      s<-zz$nbre[ia[ncq+1-i]:iz[ncq+1-i]]
      j.sp[[i]]<-sort(s); j.cq[[i]]<-sort(c(s,zz$le[ncq+1-i]))
      w[i]<-ncq+1-min(zz$elim[s])
    }
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

