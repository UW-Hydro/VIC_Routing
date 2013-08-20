/*=======================  LoadData.c ============================
	Load forcing and validation data

        Yuqiong Liu, March 2003
==================================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"

void LoadData(char *finput, char *fval, Moscem *moscem, Data_ptr data_ptr)   {

    int i, j;
    FILE *fp1, *fp2;
    char str[MAX_LINE_LEN];

    data_ptr->input_mean=0.;

    fp1 = Fopen(finput, "r");
    fp2 = Fopen(fval,"r");

    for (i=0; i<moscem->nTSteps_Inputs; i++) {
      for (j=0; j<moscem->nInputs; j++) 
	fscanf(fp1,"%lf",&(data_ptr->InputData[i][j]));
      data_ptr->input_mean+=data_ptr->InputData[i][3]; 
    }
    data_ptr->input_mean/=moscem->nTSteps_Inputs;

    //printf("LoadData mean input: %f Last input:%f Resevap:%f inputfile %s valfile %s\n",
    //	   data_ptr->input_mean,data_ptr->InputData[i-1][3],data_ptr->InputData[i-1][6],finput,fval);

    for (j=0; j<moscem->nFluxes; j++) moscem->nValTsteps[j] = 0;

    for (i=0; i<moscem->nTSteps_Fluxes; i++) {
         for (j=0; j<moscem->nInputs; j++) fscanf(fp2,"%lf", &(data_ptr->ValData[i][j]));
        for (j=0; j<moscem->nFluxes; j++) 
            if (data_ptr->ValData[i][j] != MISSING_VALUE) moscem->nValTsteps[j]++;
    }     

    fclose(fp1);
    fclose(fp2);

}
