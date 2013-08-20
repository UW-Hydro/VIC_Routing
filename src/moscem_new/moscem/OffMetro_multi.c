/*========================= OffMetro.c ====================================
         Perform Metropolis-annealing scheme on the complex "iComplex"
         for multi-objective cases

         Yuqiong Liu, March  2003
========================================================================*/
#include <stdlib.h>
#include <math.h>
#include "constant.h"
#include "datatype.h"
#include "moscem.h"
#include "utility.h" 

void OffMetro_multi(Moscem *moscem, Parameter_ptr parameter_ptr, Data_ptr data_ptr, 
     ResCar_ptr rescar_ptr, int iComplex, Files *files) {
	
    int i, j;
    int npts, npar, nobj, nopt;
    double *par1, *obj1, **pars,**cov, **sqrtm, *newPar;
    double ru, *z, *newObj, dx, **objs2,*prob, *xpar, *prob1;
    int *rank, ndom1, control, nsteps;
    SeqList_ptr newPnt;
    double mean_streamflow;
    float scalingfactor;

    nobj   = moscem->nFluxes;
    npar   = moscem->nPars;
    nopt   = moscem->nOptPar;
    npts   = moscem->nSamples/moscem->nComplex;
    par1   = DoubleVector(nopt);
    obj1   = DoubleVector(nobj);
    pars   = DoubleMatrix(npts, nopt);
    cov    = DoubleMatrix(nopt, nopt);
    z      = DoubleVector(nopt);
    sqrtm  = DoubleMatrix(nopt, nopt);
    newPar = DoubleVector(nopt);
    newObj = DoubleVector(nobj);
    objs2  = DoubleMatrix(npts+2, nobj);
    xpar   = DoubleVector(npar);
    rank   = IntVector(npts+2);
    prob   = DoubleVector(npts+2);
    prob1  = DoubleVector(npts);

    newPnt = (SeqList_ptr)malloc(sizeof(*newPnt));
    newPnt->ParValue = DoubleVector(nopt);
    newPnt->ObjValue = DoubleVector(nobj);
    newPnt->Next = NULL;
    mean_streamflow = data_ptr->input_mean;

    for (i=0; i<nopt; i++) par1[i] = SeqTail[iComplex]->ParValue[i];
    for (i=0; i<nobj; i++) obj1[i] = SeqTail[iComplex]->ObjValue[i];
 
    for (i=0; i<npts; i++)  {
	for(j=0; j<nopt; j++) 
 	    pars[i][j] = data_ptr->Complex[iComplex][i][j];
	for (j=0; j<nobj; j++) 
	    objs2[i][j] = data_ptr->Complex[iComplex][i][j+nopt];
    }

    CovMatrix(npts, nopt, pars, cov);  /* calculate covariance matrix */

    ru = UnifRand(&seed);
    for (i=0; i<nopt; i++) z[i] = NormRand(&seed);

    Sqrtm(cov, sqrtm, nopt);   /* calculate matrix square root of the covariance matrix*/

    for (i=0; i<nopt; i++) {
	dx = 0;
	for (j=0; j<nopt; j++)  dx = dx + sqrtm[i][j]*z[j];
	newPar[i] = par1[i]+dx;
    }

    control = CheckPars(moscem, parameter_ptr, par1, newPar);        /*check for physical consistancy*/
   
    CompleteParsets(moscem, parameter_ptr, newPar, xpar);
    control = ParControl(moscem, parameter_ptr, xpar);   /*check for mutual consistancy */ 

    if (control == -1)   for(i=0; i<nobj; i++) newObj[i] = INF;
    else            {

        DriveModel(moscem,xpar,data_ptr,rescar_ptr,files);
        ObjFunc(moscem, data_ptr->Output, data_ptr->ValData, 
		newObj, &(data_ptr->Iter),mean_streamflow,rescar_ptr);
	//	printf("OffMetro_multi4 %d %f %f %12.3E %12.3E\n", 
	//     nobj,obj1[0],obj1[1],newObj[0],newObj[1]);
        for (i=0; i<nobj; i++) {
	    objs2[npts][i] = obj1[i]; objs2[npts+1][i] = newObj[i];
        }
        ParetoRanking(moscem, npts+2, objs2, rank, &ndom1);
	//printf("OffMetro_multi5\n"); 
        CompProb(nobj, moscem->ObjOptFlag, npts+2, objs2, rank, prob);
        for (i=0; i<npts; i++) prob1[i] = prob[i];
        SortProb(npts, nobj, objs2, prob);
        SortProb(npts, nopt, pars, prob1);

        for (i=0; i<npts; i++)  {
            for (j=0; j<nopt; j++) 
                data_ptr->Complex[iComplex][i][j] = pars[i][j];
            for (j=0; j<nobj; j++)
                data_ptr->Complex[iComplex][i][j+nopt] = objs2[i][j];
            data_ptr->Complex[iComplex][i][nopt+nobj] = prob[i];
        }

/* -------- replace the drawn point with the new point ------------------------ */
        if ( pow( prob[npts]/prob[npts+1], 0.5*prob[npts+1]) > ru ) {  
   	    for (i=0; i<nopt; i++) par1[i] = newPar[i];
	    for (i=0; i<nobj; i++) obj1[i] = newObj[i];
        }

        for (i=0; i<nopt; i++) 
	    data_ptr->Complex[iComplex][npts-1][i] = par1[i];
        for (i=0; i<nobj; i++)
	    data_ptr->Complex[iComplex][npts-1][i+nopt] = obj1[i];
        data_ptr->Complex[iComplex][npts-1][nopt+nobj] = prob[npts+1];

        for (i=0; i<nopt; i++) newPnt->ParValue[i] = par1[i];
        for (i=0; i<nobj; i++) newPnt->ObjValue[i] = obj1[i];
        newPnt->ProbValue = prob[npts];    
        AddSeqList(iComplex, newPnt);
    }

    FreeDoubleVector(par1);
    FreeDoubleVector(obj1);
    FreeDoubleMatrix(pars,npts);
    FreeDoubleMatrix(cov,nopt);
    FreeDoubleVector(z);
    FreeDoubleMatrix(sqrtm,nopt);
    FreeDoubleVector(newPar);
    FreeDoubleVector(newObj);
    FreeDoubleMatrix(objs2,npts+2);
    FreeIntVector(rank);
    FreeDoubleVector(prob);
    FreeDoubleVector(xpar);
    FreeDoubleVector(prob1);
}
