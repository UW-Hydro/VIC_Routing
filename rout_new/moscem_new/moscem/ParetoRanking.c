/*=============================================================================  
This C program was developed to perform pareto ranking based on multi-objective
function values.   March 2003, Yuqiong Liu  
==============================================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void ParetoRanking(Moscem *moscem, int npts, double **obj, int *rank, int *ndom) {

    int i,j,k, n1,oldn, ranking, tmp, nobj1;
    double   **obj1;
    int     *idx1, *idx2, *rank1;
     
    obj1 = DoubleMatrix(npts+1, moscem->nOptObj+1);
    idx1 = IntVector(npts+1);
    rank1 = IntVector(npts+1);

    for (i=1;i<=npts;i++) rank1[i] = 0;

    k = 0;
    for (i=1;i<=moscem->nFluxes; i++) {
       if (moscem->ObjOptFlag[i-1] == 1) {
          k++;
          for (j=1;j<=npts;j++) obj1[j][k] = obj[j-1][i-1];
       }
    }

    nobj1 = k;
    n1=0;
    ranking = 1;

    while ( (npts-n1)>0 & (ranking < npts) ) {

       for (i=1; i<=npts; i++) idx1[i] = 1;
       for (i=1; i<=npts; i++)   {
           idx2 = IntVector(npts-i+1);
           for (j=1; j<=npts-i; j++) idx2[j] = 0;
           for (k=1; k<=nobj1; k++) {
               for (j=i+1; j<=npts; j++) {
                  if ( obj1[i][k] >= obj1[j][k] ) tmp = 1;
                  else    tmp = 0;
                  idx2[j-i] = idx2[j-i] + tmp;
               }
           }
           tmp = 0;
           for (j=1; j<=npts-i; j++)
               if (idx2[j] == nobj1) tmp++;
           if (tmp>0) idx1[i] = 0;
           for (j=i+1; j<=npts; j++)  {
               if (idx2[j-i] == 0) tmp=1;
               else tmp = 0;
               idx1[j] = idx1[j] - tmp;
           }
           FreeIntVector(idx2);
       }
       oldn = n1;
       n1 = 0;
       for (i=1; i<=npts; i++)  {
           if (idx1[i] == 1) {
              n1++;
              rank1[i] = ranking;
              for (j=1;j<=nobj1; j++) obj1[i][j] = INF;
           }
       }

       n1 = n1+oldn;
       if (ranking == 1) *ndom = n1; 
       ranking = ranking +1;
    }              
    
    for (i=0;i<npts;i++) rank[i] = rank1[i+1];

    FreeIntVector(idx1);
    FreeIntVector(rank1);
    FreeDoubleMatrix(obj1,npts+1);
       
}


      
