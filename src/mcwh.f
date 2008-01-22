      subroutine mcwh(nv,nn,nvals,nc,cnbr,
     &     it,inbre,iel,le,nne,nbre,
     &     nbr,wt,next)
      
      integer first,cnbr
c     integer size,totsize
      dimension nvals(nv),nn(nv),nbr(nv,nv),wt(nv),
     &     next(nv),iel(nv),cnbr(nc),le(nv),nne(nv),
     &     nbre(nc)
      
      ip = 0
      do i = 1,nv
         do j = 1,nn(i)
            ip = ip+1
            nbr(j,i) = cnbr(ip)
         end do
         do j = nn(i)+1,nv
            nbr(j,i) = 0
         end do
      end do
      
c..   init list of active vertices
      
      first = 1
      do i = 1,nv-1
         next(i) = i+1
      end do
      next(nv) = 0
      
c..   init weights
      
      do i = 1,nv
         wt(i) = 0.0
         do j = 1,nn(i)
            wt(i) = wt(i)+log(real(nvals(nbr(j,i))))
         end do
      end do
      
c..   init iel
      
      do i = 1,nv
         iel(i) = 0
      end do
      
c..   init vertex and edge count
      
      nedge = 0
      do i = 1,nv
         nedge = nedge+nn(i)
      end do
      nedge = nedge/2
      nactive = nv
      it = 0
      inbre = 0
      
c     totsize = 0
      
c..   main loop
      
      do while(nedge.lt.(nactive*(nactive-1))/2)
         
         it = it+1
         
c     write(*,*) nactive,nedge
c     write(*,*) wt
         
c..   find min weight
         
         iprev = 0
         i = first
         imin = 0
         iminprev = 0
         wmin = 0.0
         do while(i.ne.0)
            if(imin.eq.0.or.wt(i).lt.wmin) then
               imin = i
               iminprev = iprev
               wmin = wt(i)
            end if
            iprev = i
            i = next(i)
         end do
         
         if(iminprev.eq.0) then
            first = next(imin)
         else
            next(iminprev) = next(imin)
         end if
         
         i = imin
         
c     write(*,*) 'eliminate',i,' with nbrs:',(nbr(j,i),j=1,nn(i))
         
         nactive = nactive-1
         iel(i) = it
         le(it) = i
         nne(it) = nn(i)
c     size = nvals(i)
         do j = 1,nn(i)
            inbre = inbre+1
            nbre(inbre) = nbr(j,i)
c     size = size*nvals(nbr(j,i))
         end do
c     totsize = totsize+size
         
c..   remove i from nbr lists, and reduce weights
         
         do j = 1,nn(i)
            k = nbr(j,i)
            do l = 1,nn(k)
               if(nbr(l,k).eq.i) go to 1
            end do
            return
c     stop 99
 1          if(l.ne.nn(k)) nbr(l,k) = nbr(nn(k),k)
            nn(k) = nn(k)-1
            wt(k) = wt(k)-log(real(nvals(i)))
         end do
         nedge = nedge-nn(i)
         
c..   do we need to fill in?
         
         do j1 = 1,nn(i)
            k1 = nbr(j1,i)
            do j2 = 1,nn(i)
               if(j2.ne.j1) then
                  k2 = nbr(j2,i)
                  do l = 1,nn(k1)
                     if(nbr(l,k1).eq.k2) go to 2
                  end do
c     write(*,*) 'fill in',k1,k2
                  nn(k1) = nn(k1)+1
                  nn(k2) = nn(k2)+1
                  nbr(nn(k1),k1) = k2
                  nbr(nn(k2),k2) = k1
                  wt(k1) = wt(k1)+log(real(nvals(k2)))
                  wt(k2) = wt(k2)+log(real(nvals(k1)))
                  nedge = nedge+1
 2                continue
               end if
            end do
         end do
         
c..   end main loop
         
      end do
      
c     write(*,*) 'left with',first,(nbr(j,first),j=1,nn(first))
      iel(first) = it+1
      do j = 1,nn(first)
         iel(nbr(j,first)) = it+1
      end do
      
c     write(*,*) 'total of clique sizes:',totsize
      
c..   add last clique on end of separator list
      
      nne(it+1) = nn(first)+1
      inbre = inbre+1
      nbre(inbre) = first
      do j = 1,nn(first)
         inbre = inbre+1
         nbre(inbre) = nbr(j,first)
      end do
      
      return
      
      end
      
c-----------------------------------------------------------
