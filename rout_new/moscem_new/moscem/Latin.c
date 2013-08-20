/*================== Latin.c =================================
	Performs Latin-Hypercube random sampling

	Inputs:  nSample - number of random numbers to be sampled
	         nDimension - the dimension of each sample
		 Min - the lower bound of samples
	         Max - the upper bound of samples
	Outputs: Samples - the sampled random numbers between the 
	         interval [Min Max]

         Yuqiong Liu, March 2002
=========================================================== */
#include "constant.h"
#include "datatype.h"
#include "moscem.h"
#include "utility.h"

void Latin(Moscem *moscem, Parameter_ptr par_ptr, Data_ptr data)  {

    int i, j, k;
    int **index1, *index2;
    int element, nPos, kk;
    int nSample, nDimension;

    nSample = moscem->nSamples;
    nDimension = moscem->nOptPar;
    index1 = IntMatrix(nSample, nDimension);
    index2 = IntVector(nSample);
    
    for (i=0; i<nSample; i++)   {
	for (j=0; j<nDimension; j++)  index1[i][j] = 1;
	index2[i] = 0;
    }

    for (i=0; i<nSample; i++)    {
	for (j=0; j<nDimension; j++) {
            nPos = 0;
	    for (k=0; k<nSample; k++)   
	        if (index1[k][j] > 0)   {
		    index2[nPos] = (k+1)*index1[k][j];
		    nPos = nPos+1;
	        }

	    kk = -1;
	    while (kk < 1)  kk = (int)Round(0.5+nPos*UnifRand(&seed));
            element = index2[kk-1];
	    index1[element-1][j] = 0;
	    data->ParValue[i][j] = par_ptr->LowerBound[j]+(element-1+UnifRand(&seed))/nSample * 
                 (par_ptr->UpperBound[j] - par_ptr->LowerBound[j]);
            
	}
    }
    
    FreeIntMatrix(index1, nSample);
    FreeIntVector(index2);

}
