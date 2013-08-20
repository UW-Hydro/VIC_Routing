/*=======================  LoadData.c ============================
	Load forcing and validation data

        Yuqiong Liu, March 2003
==================================================================*/

#include <stdio.h>
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
        for (j=0; j<moscem->nInputs; j++) fscanf(fp1,"%lf",&(data_ptr->InputData[i][j]));
	data_ptr->input_mean+=data_ptr->InputData[i][3]; 
    }
    data_ptr->input_mean/=moscem->nTSteps_Inputs;

    fgets(str, MAX_LINE_LEN, fp2);
    for (i=0; i<moscem->nTSteps_Fluxes; i++) {
      fscanf(fp2,"%s", str);
      for (j=0; j<moscem->nFluxes; j++) fscanf(fp2,"%lf", &(data_ptr->ValData[i][j]));
    }

    fclose(fp1);
    fclose(fp2);

}

