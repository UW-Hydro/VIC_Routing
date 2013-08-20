/* ============================ Moscem.c ================================
	Main Program: this program was developed for user's convenience to 
               perform pre/post-processing before/after optimization
	
        Yuqiong Liu, yqliu@hwr.arizona.edu (520)6213973

        Last modified: March, 2003
========================================================================= */
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "postproc.h"

int main()  {

    Moscem         moscem;          /* Moscem parameters  */
    Files          files;           /* files names */
    Data_ptr       data_ptr;        /* data */
    int            i, j, k;         /* local loop indices */
    time_t         t_tot;           /* record computatition time */
    int            nrun;            /* number of model runs */
    int            flag;
    double         mean_streamflow;
/*------------------------------------------------------------- 
Initilize necessary parameters; get input and output file
names from file "postproc.in"  
--------------------------------------------------------------*/
    Initialize(&moscem, &files);
    switch (moscem.iMod)  {
         case 1:
              printf("\n###### Make a single model run for time series with default parameters ######\n");
              nrun = 1;
              break;
         case 2:
              printf("\n###### Make a single model run for time series with an optimal parameter set ######\n");
              nrun = 1;
              break; 
         case 3:
              printf("\n##### Make %4d model runs for boundary and avererage time series ####\n", moscem.nSamples);
              nrun = moscem.nSamples;
              break;
         default:
              printf("ERROR: iMod must be 1, or 2, or 3.\n");
    }

/*--------------------------------------------------------------
Memory allocation for parameter and data pointers
---------------------------------------------------------------*/
    data_ptr = (Data_ptr)malloc(sizeof(*data_ptr));
    data_ptr->InputData = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->ValData   = DoubleMatrix(moscem.nTSteps_Fluxes,moscem.nFluxes);
    data_ptr->Output    = DoubleMatrix(moscem.nTSteps_Fluxes,moscem.nFluxes);
    data_ptr->ParValue  = DoubleVector(moscem.nPars);
    data_ptr->ObjValue  = DoubleVector(moscem.nFluxes);
    
    if (moscem.iMod == 3) { 
        data_ptr->lowBound = DoubleMatrix(moscem.nTSteps_Fluxes, moscem.nFluxes);
        data_ptr->uppBound = DoubleMatrix(moscem.nTSteps_Fluxes, moscem.nFluxes);
        data_ptr->average = DoubleMatrix(moscem.nTSteps_Fluxes, moscem.nFluxes);
    }

/*----------------------------------------------------------------
Load forcing and validation data
----------------------------------------------------------------*/
    LoadData(files.InputFile, files.ValFile, &moscem, data_ptr);

    mean_streamflow=data_ptr->input_mean;

    if (moscem.iMod == 3) {
        for (i=0; i<moscem.nTSteps_Fluxes; i++) {
            for (j=0; j<moscem.nFluxes; j++)  {
                data_ptr->uppBound[i][j] = -1.E12;
                data_ptr->lowBound[i][j] = 1.E12;
                data_ptr->average[i][j]  = 0.;
            }
        }
    }

/*--------------------------------------------------------------
     Run the model, compute objective functions
---------------------------------------------------------------*/

    time(&t_tot);
    printf("------------------------------------\n");
    for (k=0; k<nrun; k++)  {
        printf("%5d   of  %5d  runs:", k+1, moscem.nSamples);
       
        if (moscem.iMod == 1) 
            LoadPardef(&moscem, &files, data_ptr->ParValue);
        else if (moscem.iMod == 2) 
            LoadParopt(&moscem, &files, data_ptr->ParValue, data_ptr->ObjValue);     
        else if (moscem.iMod == 3) {
            moscem.nParIdx = k+1;
            LoadParopt(&moscem, &files, data_ptr->ParValue, data_ptr->ObjValue);
            for (i=0; i<moscem.nFluxes;i++) {
                if (data_ptr->ObjValue[i] > moscem.ObjMax[i])  {
                    flag = 0; break;
                }
                else flag = 1;
            }
            if (flag == 0)  {
                printf("  large obj. values, filtered out.\n");
                continue;
            }
        }
        
        DriveModel(&moscem, data_ptr->ParValue, data_ptr);
        ObjFunc(&moscem, data_ptr->Output, data_ptr->ValData, 
		data_ptr->ObjValue,mean_streamflow);
        for (i=0; i<moscem.nFluxes; i++) printf("%12.3f", data_ptr->ObjValue[i]);
        printf("\n"); 

        if (moscem.iMod == 3)  {
            for (i=0; i<moscem.nTSteps_Fluxes; i++)  {
                for (j=0; j<moscem.nFluxes; j++)   {
                    data_ptr->uppBound[i][j] = max(data_ptr->uppBound[i][j], data_ptr->Output[i][j]);
                    data_ptr->lowBound[i][j] = min(data_ptr->lowBound[i][j], data_ptr->Output[i][j]);
                    data_ptr->average[i][j] = data_ptr->average[i][j] + data_ptr->Output[i][j]/nrun;
                }
            }
        }
    }

/*------------------------------------------------------------
    Write outputs to files
--------------------------------------------------------------*/
    Output(&moscem, data_ptr, &files);

    printf("\nCompleted in %10.4g SECONDS. Free meories.\n", (time(NULL)-t_tot)*1.);
    printf("---------------------------------------------------\n\n");    

    FreeDoubleVector(moscem.ObjMax);
    FreeDoubleMatrix(data_ptr->InputData,moscem.nTSteps_Inputs);
    FreeDoubleMatrix(data_ptr->ValData,  moscem.nTSteps_Fluxes);
    FreeDoubleMatrix(data_ptr->Output,   moscem.nTSteps_Fluxes);
    FreeDoubleVector(data_ptr->ParValue);
    FreeDoubleVector(data_ptr->ObjValue);

    if (moscem.iMod == 3) {
       FreeDoubleMatrix(data_ptr->uppBound, moscem.nTSteps_Fluxes);
       FreeDoubleMatrix(data_ptr->lowBound, moscem.nTSteps_Fluxes);
       FreeDoubleMatrix(data_ptr->average, moscem.nTSteps_Fluxes);
    }

    if ( data_ptr != NULL) free(data_ptr);
    
    return 0;
}

