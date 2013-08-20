/*=================================================================
     Add in the non-optimization parameters to make a complete 
     parameter set

     Yuqiong Liu, March 2003
================================================================*/


#include "constant.h"
#include "datatype.h"
#include "stdlib.h"

void CompleteParsets(Moscem *moscem, Parameter_ptr par_ptr, double *xpar, double *xpar1) {

     int i, j, npar, nopt;

     npar = moscem->nPars;
     nopt = moscem->nOptPar;
     j=0;
     for (i=0; i<npar; i++) {
         if (par_ptr->OptFlag[i] !=1 ) xpar1[i] = par_ptr->ParDefault[i];
         else { 
             xpar1[i] = xpar[j];
             j++;
         }
     }

     if (j!=nopt) {
         printf("ERROR: number of parameters for optimozation not consistent in CompleteParsets.c\n");
         exit(-1);
     } 
}

