/* ================================================================
Part the samples into complexes
Yuqiong Liu, March 2003
=================================================================*/

#include "constant.h"
#include "datatype.h"

void PartSample(Moscem *moscem, Data_ptr data_ptr)  {

    int i,j,k;
    int nobj, npar, npts, nc, idx1;

    nobj = moscem->nFluxes;
    npar = moscem->nOptPar;
    npts = moscem->nSamples;
    nc   = moscem->nComplex;

    idx1 = 0;

    for (i=0; i<(int)(npts/nc); i++) {
        for (j=0; j<nc; j++)   {
	    for (k=0; k<npar; k++)  
	        data_ptr->Complex[j][i][k] = data_ptr->ParValue[idx1][k];
	    for (k=0; k<nobj; k++)  
	        data_ptr->Complex[j][i][k+npar] = data_ptr->ObjValue[idx1][k];
	    data_ptr->Complex[j][i][npar+nobj] = data_ptr->ProbValue[idx1];

	    idx1++;
	}
    }

}
