/*============================================================================
Calculate the convergence diagnostic based on Gelman, A. and D.R. Rubin, 1992. 
Inference from Iterative Simulation Using Multiple Sequences
Statistical Science, Volume 7, Issue 4, 457-472.

Yuqiong Liu, March 2003
============================================================================*/
#include <math.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void Gelman(int npar, int nc, double *converg)  {

     int i,j, k;
     int npts, nstart, npts1;
     SeqList_ptr tmp_ptr1;
     double **tmpArr, *ave1, *varseq, **varseq1, **varseq2, **atmp, **btmp;
     double ave0, B, W, Vhat, aveseq, var0, varV, df;
     double tmp1, tmp2, tmp3;

     npts = SeqHead[0]->Index;
     if (npts > 10) 
         nstart = npts - npts/2 +1;
     else
         nstart = 1;
      
     npts1 = npts - nstart+1;

     tmpArr = DoubleMatrix(npts1, nc);
     ave1   = DoubleVector(nc);
     varseq = DoubleVector(nc);
     varseq1= DoubleMatrix(nc,2);
     varseq2= DoubleMatrix(nc,2);
     atmp   = DoubleMatrix(2,2);
     btmp   = DoubleMatrix(2,2);
     
     for (i=0; i<npar; i++)  {
         for(j=0; j<nc; j++) {
             tmp_ptr1 = SeqHead[j];
             for (k=0; k<npts; k++)   {
                 if (k >= nstart-1)  tmpArr[k-nstart+1][j] = tmp_ptr1->ParValue[i];
                 tmp_ptr1 = tmp_ptr1->Next;
             }
         } 

         Mean(npts1, nc, tmpArr, ave1);

         ave0 = 0.;
         for (j=0; j<nc; j++) ave0 = ave0 + ave1[j]/nc;

         B = 0.;
         for (j=0; j<nc; j++)  B = B + 1.0*npts1/(nc-1.0) * pow(ave1[j]-ave0, 2.0);

         Var(npts1, nc, tmpArr, varseq);
         
         W = 0.;
         for (j=0; j<nc; j++) W = W + varseq[j]/nc;

         if (W < 1.0E-10) 
             converg[i] = 1.0e+5;
         else   {
             Vhat = (npts1-1.0)/npts1 * W + B/npts1 + B/(npts1*nc);
         
             aveseq = 0.0;
             for (j=0; j<nc; j++)  { 
                 varseq1[j][0] = varseq[j]; 
                 varseq1[j][1] = pow(ave1[j], 2.0);
                 varseq2[j][0] = varseq[j];
                 varseq2[j][1] = ave1[j];

                 aveseq = aveseq + 1.0*varseq[j]/nc;
             }

             CovMatrix(nc, 2, varseq1, atmp);
             CovMatrix(nc, 2, varseq2, btmp);

             var0 = 0.;
             for (j=0; j<nc; j++) var0 = var0 + 1.0 * pow(varseq[j]-aveseq, 2.0)/(nc-1);

             tmp1 = pow((npts1-1)/npts1, 2.0) * var0/nc;
             tmp2 = pow((nc+1)/(nc*npts1), 2.0) * 2.0/(nc-1) * pow(B, 2.0);
             tmp3 = 2.0*(nc+1)*(npts-1)/pow(npts1*nc, 2.0) * npts1/nc * (atmp[0][1] - 2.0 *ave0*btmp[0][1]);
             varV = tmp1+tmp2+tmp3;         
   
             df = 2.0 *pow(Vhat,2.0)/varV;

             converg[i] = sqrt( Vhat/W*df/(df-2.0) );
         } 
     }

     FreeDoubleMatrix(tmpArr, npts1);
     FreeDoubleVector(ave1);
     FreeDoubleVector(varseq);
     FreeDoubleMatrix(varseq1, nc);
     FreeDoubleMatrix(varseq2, nc);
     FreeDoubleMatrix(atmp, 2);
     FreeDoubleMatrix(btmp, 2);
}
