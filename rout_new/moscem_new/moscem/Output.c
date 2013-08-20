/* ===============================================================
print the results (parameter set, objective function values,
and convergences to the output files

Yuqiong Liu, March  2003
================================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void Output(Moscem *moscem, Data_ptr data_ptr, Parameter_ptr par_ptr, Files *files,
	    double *final_params) {

    FILE  *fp1, *fp2, *fp3;
    int i, j;
    double *xpar, *prob1;
    CvgList_ptr tmp_ptr;

    xpar = DoubleVector(moscem->nPars);
    prob1 = DoubleVector(moscem->nSamples);

    for (i=0; i<moscem->nSamples; i++) prob1[i] = data_ptr->ProbValue[i];
    SortProb(moscem->nSamples, moscem->nFluxes, data_ptr->ObjValue, data_ptr->ProbValue);
    SortProb(moscem->nSamples, moscem->nOptPar, data_ptr->ParValue, prob1);

    fp1 = Fopen(files->ObjOutFile,"w");
    for (i=0; i<moscem->nSamples; i++) {
        fprintf(fp1,"%5d\t", i+1);
        for (j=0; j<moscem->nFluxes; j++) fprintf(fp1,"%10.3f ",data_ptr->ObjValue[i][j]);
        fprintf(fp1,"%10.3f\n", data_ptr->ProbValue[i]);
    }
    fclose(fp1);

    fp2 = Fopen(files->ParOutFile,"w");
    for (i=0; i<moscem->nSamples; i++) {
        fprintf(fp2,"%5d", i+1);
        CompleteParsets(moscem, par_ptr, data_ptr->ParValue[i], xpar);
        for (j=0; j<moscem->nPars; j++) {
	  fprintf(fp2," %.3f\t",xpar[j]);
	  if(i==0) final_params[j]=xpar[j];
	}
        fprintf(fp2,"\n");
    }
    fclose(fp2);
    

    fp3 = Fopen(files->CvgOutFile,"w");
    tmp_ptr = CvgHead;
    for (i=0; i<CvgHead->Index; i++) {
        fprintf(fp3,"%5d %8d", i+1, tmp_ptr->Iter);
        for (j=0; j<moscem->nOptPar; j++) fprintf(fp3, "%12.4f", tmp_ptr->Convergence[j]);
        fprintf(fp3,"\n");
        tmp_ptr = tmp_ptr->Next;
    }
    fclose(fp3);   

    FreeDoubleVector(xpar);
    FreeDoubleVector(prob1);
}
