/**======================= CompProb.c =========================
Calculate fitness based on pareto ranking for multi-obj cases
For single-obj cases, just assign the optimized obj function 
value to the probability

Yuqiong Liu, March 2003
===============================================================**/
#include "constant.h"
#include "utility.h"
#include "datatype.h"
#include "moscem.h"

void CompProb(int nobj,int *optIdx,int ns, double **obj,int *rank, double *prob) {

    int i,j;

#if MOD_ALGO == MULTI
    int k,jj,kk;
    int max_rank, *idx1, *idx2, n1, nopt;
#endif
	
#if MOD_ALGO == SINGL
    for (i=0; i<nobj; i++) {
        if (optIdx[i] == 1) break;
    }
    for (j=0; j<ns; j++) prob[j] = obj[j][i];
  

#elif MOD_ALGO == MULTI

    idx1 = IntVector(ns);
    idx2 = IntVector(ns);

    max_rank = 0;
    nopt = 0;
    for (i=0; i<nobj; i++) 
	if(optIdx[i] == 1) nopt++;

    for (i=0; i<ns; i++) 
	if (rank[i] > max_rank) max_rank = rank[i];

    for (i=1; i<=max_rank; i++) {
	k = 0;
	for (j=0; j<ns; j++)  {
	    if (rank[j] == i) {idx1[k] = j; k++;}
	}
	for (j=0; j<k; j++) {
	    n1 = 0;
	    for (kk=0; kk<ns; kk++) {
		idx2[kk] = 0;
		for (jj=0; jj<nobj; jj++) {
		    if ((obj[idx1[j]][jj] < obj[kk][jj]) &(optIdx[jj] == 1)) idx2[kk]++;
		}
		if (idx2[kk] == nopt) n1++;
	    }
	    prob[idx1[j]] = (i-1) + n1*1.0/(ns+1);
	}

    }

    FreeIntVector(idx1);
    FreeIntVector(idx2);

#endif
}
