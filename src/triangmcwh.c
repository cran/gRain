#include <string.h>
#include <stdlib.h>
#include <Rdefines.h>

void printvec(int *x, int *n){

  for (int i=0; i<*n; i++)
    Rprintf(" %2i ",x[i]);
  Rprintf("\n");
}

void printmat(int **Avec, int *nvar, char **vnames, int *nlev){
  int ii, jj;
  
  Rprintf("   ");
  for (ii=0; ii<*nvar; ii++){
    Rprintf(" %2s ", vnames[ii]);
  }
  Rprintf("\n");
  for (ii=0; ii<*nvar; ii++){
    Rprintf(" %s ", vnames[ii]);
    for (jj=0; jj<*nvar; jj++){
      Rprintf(" %2i ", Avec[ii + *nvar * jj]);
    }
    Rprintf("\n");
  }
  Rprintf(" ---------------------------\n");
}


void nbfun(int **Avec, int *nvar, int varidx, int *active, int *nb, int *nne){
  *nne = 0;
  for (int jj=0; jj<*nvar; jj++){
    if (active[jj]==1){
      nb[jj] = abs( (int) Avec[varidx + *nvar * jj]);
      *nne   = *nne + nb[jj];
    } else {
      nb[jj] = 0;
    }
  }
}

void nedgesfun (int **Avec, int *nvar, int *nb, int *ans){
  *ans = 0;
  //int tmp;
  //Rprintf("nedgesfun: ");
  //for (int ii=0; ii<(*nvar-1); ii++){
  //  if (nb[ii]!=0)
  //    Rprintf("%i %i||", ii, nb[ii]);
  //}
  //Rprintf("\n");

  for (int ii=0; ii<(*nvar-1); ii++){
    if (nb[ii]!=0){
      for (int jj=ii+1; jj<*nvar; jj++){
	if (nb[jj]!=0){
	  //tmp = abs( (int) Avec[ii + *nvar * jj] );
	  //Rprintf("tmp: %i ", tmp);
	  *ans = *ans + abs( (int) Avec[ii + *nvar * jj] );
	  //Rprintf("nbii %i nbjj %i val %i\n", nb[ii], nb[jj], (int) Avec[ii + *nvar * jj]);
	}
      }
    }
  }
  //Rprintf("ans (nedgesfun): %i\n", *ans); 
}

void triangmcwh(int **Avec, int *nvar, char **vnames, int *nlev, int *ans){
  int ii, i;
  int active[*nvar], nb[*nvar];
  int nne, nedges=0;
  int totedges, nfillin;
  int goon = 0;
  int mincqidx=0, minfillin=9999;
  float mincqsize, cqsize, lognlev[*nvar];
  
  // Initialize
  float statespace = 0;
  for (ii=0; ii<*nvar; ii++) {
    active[ii]   = 1;
    //cqweight[ii] = 0;
    lognlev[ii]  = log(nlev[ii]);
    statespace   = statespace + lognlev[ii];
    //Rprintf(" %s %i %f \n", vnames[ii], nlev[ii], statespace);
  }

  //Rprintf("Matrix (start) \n"); printmat(Avec, nvar, vnames, nlev);
  //Rprintf("log statespace %f\n", statespace);
  while (goon<*nvar) {
    goon++;
    mincqsize = statespace; 
    //Rprintf("Active: "); printvec(active, nvar);
    for (ii=0; ii<*nvar; ii++){
      
      if (active[ii]==1){ 
	//Rprintf("Node %s %f \n", vnames[ii], mincqsize);
	nbfun(Avec, nvar, ii, active, nb, &nne); // nb: neighbours (in active set), nne: # neighbours (in active set)
      	//Rprintf(" ne: "); printvec(nb, nvar);  	
	nedgesfun(Avec, nvar, nb, &nedges);      // nedges: # edges between vars in nb
	
	cqsize = lognlev[ii];  
	for (i=0; i<*nvar; i++){
	  if (nb[i]!=0)
	    cqsize = cqsize + lognlev[i];
	}
	
	totedges = (int) (nne-1)*nne / 2;
	nfillin = totedges - nedges;
	
	if ( (nfillin==0) || (cqsize < mincqsize) ){	  
	  mincqsize = cqsize;
	  mincqidx  = ii;
	  minfillin = nfillin;
	}
	//Rprintf("Node %10s nne %2i totedges %2i nedges %2i nfillin %2i cqsize %5.2f  mincqvar %s mincqidx %i\n", 
	//vnames[ii], nne, totedges, nedges, nfillin, cqsize, vnames[mincqidx], mincqidx);

	if (nfillin==0)
	  break;
      }
    }
    //Rprintf("Chosen var: %s nfillin %i \n----\n", vnames[mincqidx], minfillin);
    active[mincqidx] = 0;    
    
    if (minfillin>0){
      nbfun(Avec, nvar, mincqidx, active, nb, &nne); 
      nedgesfun(Avec, nvar, nb, &nedges); 
      //Rprintf("Matrix (before fillin) \n"); printmat(Avec, nvar, vnames, nlev);      
      //Rprintf(" NB: %i >>", nne); printvec(nb, nvar);
      
      for (int ii=0; ii<*nvar; ii++){
	if (nb[ii]!=0){
	  for (int jj=0; jj<*nvar; jj++){
	    if (nb[jj]!=0){
	      if ((int) Avec[ii + *nvar * jj] == 0){
		Avec[ii + *nvar * jj] = (int*) -1;
		Avec[jj + *nvar * ii] = (int*) -1;
	      }
	    }
	  }
	}
      }
      //Rprintf("Matrix (after fillin) \n"); printmat(Avec, nvar, vnames, nlev);           
    }
    /*     for (int ii=0; ii<*nvar; ii++){ */
    /*       for (int jj=0; jj<*nvar; jj++){ */
    /* 	if ((int) Avec[ii + *nvar * jj] != 0){ */
    /* 	  Rprintf("ii %i jj %i val %i\n", ii, jj, (int) Avec[ii + *nvar * jj]); */
    /* 	} */
    /*       } */
    /*     } */
    
  }
}
















