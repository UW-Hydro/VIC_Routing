/*=============================================================
Calculate covariance matrix of the parameter sets (x[m][n]) in 
a complex.

Yuqiong Liu, March 2003
=============================================================*/

#include "constant.h"
#include "utility.h"


void CovMatrix(int m, int n, double **x, double **cov)  {


    int i,j,k;
    double *v, *ave;

    v = DoubleVector(m);
    ave = DoubleVector(m);

    Mean(m, n, x, ave);

    for (i=0; i<n; i++) 
        for (j=0; j<n; j++)   cov[i][j] = 0.; 

    for (i=0; i<n; i++) {
	for (j=i; j<n; j++) {
	    for(k=0; k<m; k++) 
		cov[i][j] = cov[i][j] + (x[k][i]-ave[i])*(x[k][j]-ave[j]);
	    cov[i][j] = cov[i][j]/((m-1) * 1.0);
	    cov[j][i] = cov[i][j];
	}
    }

    FreeDoubleVector(v);
    FreeDoubleVector(ave);
}
