/*===================================================================
Call "Gelman" to calculate the convergence and check if it meets
the convergence criteria

Yuqiong Liu, March  2003
====================================================================*/ 

#include <math.h>
#include <stdlib.h>
#include "constant.h"
#include "utility.h"
#include "datatype.h"
#include "moscem.h"

#define NL 26

int Convergence(Moscem *moscem, int iter) {

     int i, j,flag;
     double *converg, *tmp, *max_conv, *min_conv, *delConv, max_delconv;
     CvgList_ptr cvg_ptr, tmp_ptr;

     converg = DoubleVector(moscem->nOptPar);
     tmp = DoubleVector(NL); 
     max_conv = DoubleVector(moscem->nOptPar);
     min_conv = DoubleVector(moscem->nOptPar);
     delConv = DoubleVector(moscem->nOptPar);

     cvg_ptr = (CvgList_ptr)malloc(sizeof(*cvg_ptr));
     cvg_ptr->Convergence = DoubleVector(moscem->nOptPar);
     cvg_ptr->Next = NULL;

     Gelman(moscem->nOptPar, moscem->nComplex, converg);
 
     for (i=0; i<moscem->nOptPar; i++) cvg_ptr->Convergence[i] = converg[i]; 
     cvg_ptr->Iter = iter;
     AddCvgList(cvg_ptr);

     if (CvgHead->Index >= NL)  {
         for(i=0; i<moscem->nOptPar; i++)  {
             tmp_ptr = CvgHead;
             for (j=0; j<CvgHead->Index; j++) {
                 if (j>=CvgHead->Index-NL) tmp[j-CvgHead->Index+NL] = tmp_ptr->Convergence[i];
                 tmp_ptr=tmp_ptr->Next;
             }
             max_conv[i] = Max(NL, tmp);
             min_conv[i] = Min(NL, tmp);
             delConv[i] = max_conv[i] - min_conv[i];
         }
         max_delconv = Max(moscem->nOptPar, delConv);
         for(i=0; i<moscem->nOptPar; i++)  {
             if (converg[i] >= 1.2 || max_delconv > 0.001)
             {    flag = -1; break;}
             else flag = 0;
         }
     }
     else flag = -1;

     FreeDoubleVector(converg);
     FreeDoubleVector(tmp);
     FreeDoubleVector(max_conv);
     FreeDoubleVector(min_conv);
     FreeDoubleVector(delConv);

     return flag;
}

