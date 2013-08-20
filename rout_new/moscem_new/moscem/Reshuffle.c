/* ============================================================
unpack all the complexes for reshuffling
Yuqiong Liu, March  2003
==============================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"

void Reshuffle(Moscem *moscem, Data_ptr data_ptr) {


    int i, j, k;
    int nobj, npar, npts, nc, idx1, m;

    nobj = moscem->nFluxes;
    npar = moscem->nOptPar;
    npts = moscem->nSamples;
    nc   = moscem->nComplex;

    m = npts/nc;
    idx1 = 0;
    for (i=0; i<nc; i++)  {
	for (j=0; j<m; j++) {
	   for (k=0; k<npar; k++) data_ptr->ParValue[idx1][k] = data_ptr->Complex[i][j][k];
	   for (k=0; k<nobj; k++) data_ptr->ObjValue[idx1][k] = data_ptr->Complex[i][j][k+npar];
           data_ptr->ProbValue[idx1] = data_ptr->Complex[i][j][npar+nobj];
	   idx1++;
        }
    }

    if (idx1 != npts) 
	PrintError("No of points after reshuffling not consistent in 'Reshuffle'!");

}
