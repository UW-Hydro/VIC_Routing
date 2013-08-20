/* ===========================================================
Sort the points from low fitness to high fitness

Yuqiong Liu, March 2003
============================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"

void SortProb(int m, int n, double **data, double *prob)  {

     int i,j;
     int *idx, *idx1;
     double **data1, *prob1;

     idx = IntVector(m);
     idx1 = IntVector(m);
     data1 = DoubleMatrix(m, n);
     prob1 = DoubleVector(m);

     for( i = 0; i < m; i++ )  {
         idx1[i] = 0;
         for (j=0; j<n; j++) data1[i][j] = data[i][j];
         prob1[i] = prob[i];
     }

     for( i = 0; i < m-1; i++ )   {
	 for( j = i+1; j < m; j++ )  {
	     if( prob[i] <= prob[j] )
		 idx1[j] += 1;
	     else
		 idx1[i] += 1;
	 }
     }

     for (i=0; i<m; i++)   idx[idx1[i]] = i;
     for( i=0; i<m; i++ )  {
         for (j=0; j<n; j++) data[i][j] = data1[idx[i]][j];
         prob[i] = prob1[idx[i]];
     }
	
     FreeIntVector(idx);
     FreeIntVector(idx1);
     FreeDoubleVector(prob1);
     FreeDoubleMatrix(data1, m);
}
