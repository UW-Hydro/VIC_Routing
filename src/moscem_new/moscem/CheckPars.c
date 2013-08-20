/* ==========================================================
Function that checks parameters for physical consistance:
lower and upper boundaries

Yuqiong Liu, March  2003
============================================================*/

#include "constant.h"
#include "datatype.h"
#include "moscem.h"

int CheckPars(Moscem *moscem, Parameter_ptr par,double *OldPar, double *NewPar) {
	
    int i, flag;

#if MOD_ALGO == MULTI
    for (i=0; i<moscem->nOptPar; i++) {
        if (NewPar[i] < par->LowerBound[i]) NewPar[i] = 0.5*(OldPar[i]+par->LowerBound[i]);
        if (NewPar[i] > par->UpperBound[i]) NewPar[i] = 0.5*(OldPar[i]+par->UpperBound[i]);
    }
    return 0;

#elif MOD_ALGO == SINGL
    flag = 0;
    for (i=0; i<moscem->nOptPar; i++) {
        if (NewPar[i] < par->LowerBound[i]) {flag = -1; break; }
        else if (NewPar[i] > par->UpperBound[i]) {flag = -1; break;}
    }
    return flag;
#endif
}
