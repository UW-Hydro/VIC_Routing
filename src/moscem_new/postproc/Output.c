/* ===============================================================
print the results (objective function values, time series, and
the trade-off boundaires to the output files

Yuqiong Liu, March 2003
================================================================*/
#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"

void Output(Moscem *moscem, Data_ptr data_ptr, Files *files) {

    FILE  *fp1;
    int i, j;

    if (moscem->iMod == 1 || moscem->iMod == 2) {
        fp1 = Fopen(files->TsOutFile,"w");
        for (i=0; i<moscem->nTSteps_Fluxes; i++) {
           fprintf(fp1,"%5d ", i);
           /*for (j=0; j<moscem->nFluxes; j++) fprintf(fp1,"%.3f ",data_ptr->ValData[i][j]);*/
	   /* for (j=0; j<moscem->nFluxes; j++)*/ 
	      fprintf(fp1,"%.3f %.3f",data_ptr->InputData[i][3],data_ptr->Output[i][0]);
           fprintf(fp1,"\n");
        }
        fclose(fp1);

        if (moscem->iMod == 1) {
           fp1 = Fopen(files->ObjOutFile,"w");
           for (j=0; j<moscem->nFluxes; j++) fprintf(fp1,"%.3f ",data_ptr->ObjValue[j]);
           fprintf(fp1,"\n");
	   fclose(fp1);
	} 
    }
    else if (moscem->iMod == 3) {
       fp1 = Fopen(files->TrdoffFile,"w");
        for (i=0; i<moscem->nTSteps_Fluxes; i++) {
           fprintf(fp1,"%5d", i);
           for (j=0; j<moscem->nFluxes; j++) fprintf(fp1,"%15.5E",data_ptr->lowBound[i][j]);
           for (j=0; j<moscem->nFluxes; j++) fprintf(fp1,"%15.5E",data_ptr->uppBound[i][j]); 
           for (j=0; j<moscem->nFluxes; j++) fprintf(fp1,"%15.5E",data_ptr->average[i][j]);

           fprintf(fp1,"\n");
        } 
        fclose(fp1);
    }
}
