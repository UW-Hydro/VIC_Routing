/*================================================================
Initialize the starting points of parall sequences

Yuqiong Liu, March 2003
================================================================*/
#include <stdlib.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void InitSequence(Moscem *moscem, Data_ptr data_ptr)  {

     int i,j;
     int nobj, npar, npts, nc;
     SeqList_ptr     *point1;

     nobj = moscem->nFluxes;
     npar = moscem->nOptPar;
     npts = moscem->nSamples;
     nc   = moscem->nComplex;

     point1 = (SeqList_ptr *)malloc(nc*sizeof(SeqList_ptr));
     for (i=0; i<nc; i++) {
         point1[i] = (SeqList_ptr) malloc(sizeof(*point1[i]));
         point1[i]->ParValue = DoubleVector(npar);  
         point1[i]->ObjValue = DoubleVector(nobj);
         point1[i]->Next = NULL;
     }

     for (i=0; i<nc; i++) {
	 for(j=0; j<npar; j++)
	     point1[i]->ParValue[j] = data_ptr->ParValue[i][j];
	 for(j=0; j<nobj; j++)
	     point1[i]->ObjValue[j] = data_ptr->ObjValue[i][j];
	 point1[i]->ProbValue = data_ptr->ProbValue[i];

         AddSeqList(i, point1[i]);
    }

}
