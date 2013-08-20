/*========================= OffMetro.c ====================================
         Perform Metropolis-annealing scheme on the complex "iComplex"
         for single-objective cases

         Yuqiong Liu, March  2003
========================================================================*/

#include <stdlib.h>
#include <math.h>
#include "constant.h"
#include "datatype.h"
#include "moscem.h"
#include "utility.h" 

void OffMetro_singl(Moscem *moscem, Parameter_ptr parameter_ptr, Data_ptr data_ptr, 
     ResCar_ptr rescar_ptr,int iComplex, Files *files) {
	
    int i, j;
    int npts, npar, nobj, nopt;
    double *par1, *obj1, **pars, **objs, **cov, **sqrtm, *newPar;
    double ru, *z, *newObj, dx, *parMean,*prob, *xpar, *prob1;
    int control, mp, optIdx, nsteps, s, L;
    double JumpRate, threshold, ratio;
    SeqList_ptr newPnt, tmp_ptr;
    double pMeanCom, pMeanSeq, Gamma;
    double mean_streamflow;

    Gamma = 0.;
    s = SeqHead[iComplex]->Index; 

    nobj   = moscem->nFluxes;
    npar   = moscem->nPars;
    nopt   = moscem->nOptPar;
    npts   = moscem->nSamples/moscem->nComplex;
    L      = npts/5;

    par1   = DoubleVector(nopt);
    obj1   = DoubleVector(nobj);
    pars   = DoubleMatrix(npts, nopt);
    objs   = DoubleMatrix(npts, nopt);
    cov    = DoubleMatrix(nopt, nopt);
    z      = DoubleVector(nopt);
    sqrtm  = DoubleMatrix(nopt, nopt);
    newPar = DoubleVector(nopt);
    newObj = DoubleVector(nobj);
    parMean  = DoubleVector(nopt);
    xpar   = DoubleVector(npar);
    prob   = DoubleVector(npts);
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
        for(j=0; j<nobj; j++)
            objs[i][j] = data_ptr->Complex[iComplex][i][j+nopt];
        prob[i] = data_ptr->Complex[iComplex][i][nopt+nobj];
        prob1[i] = prob[i];
    }
    Mean(npts, nopt, pars, parMean);    /* calulate the mean of the parameter sets */
    CovMatrix(npts, nopt, pars, cov);  /* calculate covariance matrix */

    JumpRate = pow(2.4/sqrt(nopt), 2.0);
    for (i=0; i<nopt; i++) 
        for(j=0; j<nopt; j++) cov[i][j] = cov[i][j]*JumpRate;
    Sqrtm(cov, sqrtm, nopt);   /* calculate matrix square root of the covariance matrix*/

    control = -1;
    threshold = 1.0E+7;

    SortProb(npts, nobj, objs, prob);
    SortProb(npts, nopt, pars, prob1);
        
    for (i=0; i<npts; i++)  {
        for (j=0; j<nopt; j++)   
            data_ptr->Complex[iComplex][i][j] = pars[i][j];
        for (j=0; j<nobj; j++)
            data_ptr->Complex[iComplex][i][j+nopt] = objs[i][j];
        data_ptr->Complex[iComplex][i][nopt+nobj] = prob[i];
    }

    pMeanCom = 0.;
    for (i=0;i<npts;i++) pMeanCom = pMeanCom + prob[i]/(npts*1.0);
    if ( s >= npts+L+1) {
        tmp_ptr = SeqHead[iComplex];        
        for (i=0; i<s-L-1; i++)  {
            if (i>=s-L-npts-1) prob1[i-s+L+npts+1] = tmp_ptr->ProbValue;
            tmp_ptr= tmp_ptr->Next;
        }
        pMeanSeq = 0.;
        for (i=0; i<npts; i++) pMeanSeq = pMeanSeq + prob1[i]/(npts*1.0);
    }
    else pMeanSeq = pMeanCom;

    for (j=0; j<nobj; j++)  
        if (moscem->ObjOptFlag[j] == 1) break;
    optIdx = j;
 
    while (control == -1) {
         for (i=0; i<nopt; i++) z[i] = NormRand(&seed);
         for (i=0; i<nopt; i++) {
             dx = 0;
             for (j=0; j<nopt; j++)  dx = dx + sqrtm[i][j]*z[j];
             if (pow(pMeanCom/pMeanSeq, (-1.0)*moscem->nValTsteps[optIdx]*(1.0+Gamma)/2.0 )>threshold )
                 newPar[i] = parMean[i]+dx;
             else
                 newPar[i] = par1[i]+dx;
             ratio = pow(prob[0]/prob[npts-1], (-1.0)*moscem->nValTsteps[optIdx]*(1.0+Gamma)/2.0);
         }
         control = CheckPars(moscem, parameter_ptr, par1, newPar);  /*check for physical consistancy*/
    }

    CompleteParsets(moscem, parameter_ptr, newPar, xpar);
    control = ParControl(moscem, parameter_ptr, xpar);   /*check for mutual consistancy */ 

    if (control == -1)   for(i=0; i<nobj; i++) newObj[i] = INF;
    else            {
        DriveModel(moscem, xpar, data_ptr,rescar_ptr,files);
        ObjFunc(moscem, data_ptr->Output, data_ptr->ValData, newObj, &(data_ptr->Iter),
		mean_streamflow,rescar_ptr);

/* -------- replace the drawn point with the new point ------------------------ */

        ru = UnifRand(&seed);
        if (pow(newObj[optIdx]/obj1[optIdx], (-1.0)*moscem->nValTsteps[optIdx]*(1.0+Gamma)/2.0) > ru) {
   	    for (i=0; i<nopt; i++) par1[i] = newPar[i];
	    for (i=0; i<nobj; i++) obj1[i] = newObj[i];
            mp=1;
        }
        else mp=npts;

        for (i=0; i<nopt; i++) 
	    data_ptr->Complex[iComplex][mp-1][i] = par1[i];
        for (i=0; i<nobj; i++)
	    data_ptr->Complex[iComplex][mp-1][i+nopt] = obj1[i];
        data_ptr->Complex[iComplex][mp-1][nopt+nobj] = obj1[optIdx];

        for (i=0; i<nopt; i++) newPnt->ParValue[i] = par1[i];
        for (i=0; i<nobj; i++) newPnt->ObjValue[i] = obj1[i];
        newPnt->ProbValue = obj1[optIdx];    
        AddSeqList(iComplex, newPnt);
    }

    FreeDoubleVector(par1);
    FreeDoubleVector(obj1);
    FreeDoubleMatrix(pars,npts);
    FreeDoubleMatrix(objs,npts);
    FreeDoubleMatrix(cov,nopt);
    FreeDoubleVector(z);
    FreeDoubleMatrix(sqrtm,nopt);
    FreeDoubleVector(newPar);
    FreeDoubleVector(newObj);
    FreeDoubleVector(parMean);
    FreeDoubleVector(prob);
    FreeDoubleVector(xpar);
    FreeDoubleVector(prob1);
}
