/* ============================ Moscem.c ==========================================
Main Program: perform optimization using Multi-Objective Shuffled Complex
Evolution Metropolis algorithm (MOSCEM) or Single-objective SCEM

Coded by Yuqiong Liu at the University of Arizona, 
based on the matlab code developed by Jasper A. Vrugt

References: 
Jasper A. Vrugt, Hoshin V. Gupta, Willem Bouten, and Soroosh Sorooshian,2003:
A shuffled complex evolution Metropolis algorithm for optimization and uncertainty 
assessment of hydrological model parameters. Water Resources Research. In press.

Jasper Vrugt, Hoshin Vijai Gupta, Luis A Bastidas, Willemm Bouten, and Soroosh 
Sorooshain, 2003: Effective and efficient algorithm for multi-objective optimization
of hydrologic models. Water Resources Research. In press.

yqliu@hwr.arizona.edu (520)6213973           March 2003
========================================================================= */
#include <stdlib.h>
#include <time.h>
#include "constant.h"
#include "datatype.h"
#include "moscem.h"
#include "utility.h"

SeqList_ptr     *SeqHead;
SeqList_ptr     *SeqTail;
CvgList_ptr     CvgHead;
CvgList_ptr     CvgTail;

int            seed;

int main()  {

    Moscem         moscem;          /* define pointer structures */
    Files          files;
    Parameter_ptr  parameter_ptr;
    Data_ptr       data_ptr;
    ResCar_ptr     rescar_ptr;
    double         *xpar, *prob1;  /* temporary varaible */
    double         *final_params; 
    double         final_output[NTSTEP1][NINPUT];
    double         final_input[NTSTEP1][NINPUT];
    float          scalingfactor;
    int            npts;           /* no of points in each complex */
    int            i, j, sloop;    /* local loop indices */
    int            converged;      /* check if converged */
    time_t         t_tot;         /* record computation time */

    seed = IDUM;
/*------------------------------------------------------------- 
Initilize necessary MOSCEM parameter; get input and output file
names from files "moscem.in"
--------------------------------------------------------------*/
    MoscemInit(&moscem, &files);
    moscem.ObjOptFlag = IntVector(moscem.nFluxes);
    GetObjOptFlag(files.ObjControlFile, &moscem);

    moscem.nValTsteps = IntVector(moscem.nFluxes);
/*--------------------------------------------------------------
Memory allocation for parameter and data pointers, complexes, 
initial points of each parallel sequences, based on MOSCEM input
parameters
---------------------------------------------------------------*/
    parameter_ptr = (Parameter_ptr)malloc(sizeof(*parameter_ptr));
    parameter_ptr->ParDefault  = DoubleVector(moscem.nPars);
    parameter_ptr->UpperBound  = DoubleVector(moscem.nOptPar);
    parameter_ptr->LowerBound  = DoubleVector(moscem.nOptPar);
    parameter_ptr->ParNumber   = IntVector(moscem.nPars);
    parameter_ptr->OptFlag     = IntVector(moscem.nPars);
    parameter_ptr->ParName     = CharMatrix(moscem.nPars, PAR_NAME_LEN);
    parameter_ptr->ParLongName = CharMatrix(moscem.nPars, PAR_LNAME_LEN);

    data_ptr = (Data_ptr)malloc(sizeof(*data_ptr));
    data_ptr->InputData = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->ValData   = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->Output    = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->OutputHydro = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->OutputFlood = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->OutputMean = DoubleMatrix(moscem.nTSteps_Inputs,moscem.nInputs);
    data_ptr->ParValue  = DoubleMatrix(moscem.nSamples, moscem.nOptPar);
    data_ptr->ObjValue  = DoubleMatrix(moscem.nSamples, moscem.nFluxes);
    data_ptr->ProbValue = DoubleVector(moscem.nSamples);
    data_ptr->RankIdx   = IntVector(moscem.nSamples);

    npts = moscem.nSamples/moscem.nComplex;
    data_ptr->Complex = Double3Dim(moscem.nComplex,npts, moscem.nOptPar+moscem.nFluxes+1);

    rescar_ptr = (ResCar_ptr)malloc(sizeof(*rescar_ptr));

    SeqHead = (SeqList_ptr *) malloc(moscem.nComplex*sizeof(SeqList_ptr));
    SeqTail = (SeqList_ptr *) malloc(moscem.nComplex*sizeof(SeqList_ptr));
    for (i=0; i<moscem.nComplex; i++) {
        SeqHead[i] = NULL;
        SeqTail[i] = NULL;
    }
   
    CvgHead = NULL;
    CvgTail = NULL;

    xpar = DoubleVector(moscem.nPars);
    final_params = DoubleVector(moscem.nPars);
    prob1 = DoubleVector(moscem.nSamples);
 
    time(&t_tot);
/*----------------------------------------------------------------
Define parameter lower and upper boundaries; Load forcing and 
validation data
----------------------------------------------------------------*/
    printf("\n-----------Parameter Initialization----------\n");
    ParameterInit(files.ParControlFile, files.ResCarFile,&moscem, parameter_ptr, rescar_ptr);
    LoadData(files.InputFile, files.ValFile, &moscem, data_ptr);

/*----------------------------------------------------------------
Latin-Hypercube random sampling; compute objective function values
for each sample of the entire population
-----------------------------------------------------------------*/
    Latin(&moscem, parameter_ptr, data_ptr);
    data_ptr->Iter = 0;

    printf("\n--------- Objective function values of valid initial samples ---------\n");
	CompObj(&moscem, data_ptr, parameter_ptr, rescar_ptr, &files);
    printf("\n--------- Objective function values of valid initial samples ---------\n");

    data_ptr->ndom = 0;
    sloop = 0;
/*---------------------------------------------------------------
Begin multi-objective shuffled complex evolution process
---------------------------------------------------------------*/    
    converged = -1; 
    printf("\n------------- Begin metropolis shuffled complex evolution --------------\n");
    while (1) {

/* ---------------------------------------------------------------
Stop the optimization process when number of function evaluations
reaches the specified maximum or sequences converged
-----------------------------------------------------------------*/
         if (data_ptr->Iter >= moscem.nMaxDraw || converged == 0) {
             if (data_ptr->Iter >= moscem.nMaxDraw)
                  printf("\nNumber of function evaluations reseaches the specified maximum:%10d. Stop the algorithm.\n",
                            moscem.nMaxDraw);
             else if (converged == 0)
                  printf("\nThe Sequences have converged to a sufficiently limited region. Stop the algorithm.\n");
              
             break;
         }

/*----------------------------------------------------------------
Compute conventional pareto rank members; perform the new fintness
assignment; and sort the points in order of decreasing probability
-----------------------------------------------------------------*/
#if MOD_ALGO == MULTI
         ParetoRanking(&moscem, moscem.nSamples,data_ptr->ObjValue,
             data_ptr->RankIdx, &(data_ptr->ndom));   
#endif

	 CompProb(moscem.nFluxes, moscem.ObjOptFlag, moscem.nSamples,
             data_ptr->ObjValue, data_ptr->RankIdx, data_ptr->ProbValue);
 
         for (i=0; i<moscem.nSamples; i++) prob1[i] = data_ptr->ProbValue[i];
	 SortProb(moscem.nSamples, moscem.nFluxes, data_ptr->ObjValue, data_ptr->ProbValue);
         SortProb(moscem.nSamples, moscem.nOptPar, data_ptr->ParValue, prob1);

/* -------------------------------------------------------
print info of each shuffled loop to the screen 
---------------------------------------------------------*/
         printf("SLOOP = %5d,    Iter = %8d, Best Objs = ", sloop,data_ptr->Iter);
         for (i=0; i<moscem.nFluxes; i++) printf(" %.5f",data_ptr->ObjValue[0][i]);
         printf("\n");

/*------------------------------------------------------------------
Initialize the initial points, devide the entire population into 
complexes, perform metropolis-annealing scheme a number of times
for each complex, and then unpack all complexes back into the whole
population for a new sequence evolution
-------------------------------------------------------------------*/
         if (sloop==0) InitSequence(&moscem, data_ptr);

	 PartSample(&moscem, data_ptr);

	 for (i=0; i<moscem.nComplex; i++)   {
#if MOD_ALGO == MULTI
             for (j=0; j<moscem.nOptPar; j++)  
		 OffMetro_multi(&moscem, parameter_ptr,data_ptr,rescar_ptr,i, &files);
#elif MOD_ALGO == SINGL
             for (j=0; j<moscem.nSamples/moscem.nComplex/5; j++)
                 OffMetro_singl(&moscem, parameter_ptr,data_ptr,rescar_ptr,i, &files);
#endif
	 }
        
         converged = Convergence(&moscem, data_ptr->Iter);

	 Reshuffle(&moscem, data_ptr);
         sloop++; 
    }

/*----------------------------------------------------------------
Print required output to files;
Free allocated memory before exiting the program
----------------------------------------------------------------*/

    printf("\nWriting output files\n");

    Output(&moscem, data_ptr, parameter_ptr, &files, final_params);

    printf("\nfinal parameters:\n");
    for(i=0;i<12;i++) printf("%.3f ",final_params[i]);
    printf("\n");
    printf("Scaling factor %.2f\n",final_params[12]);
    printf("Mean inflow %f (m3/day)\n",data_ptr->input_mean);
    for(i=0;i<NTSTEP1;i++) {
      for(j=0;j<NINPUT;j++) {
      final_input[i][j]=data_ptr->InputData[i][j];
      }
      final_output[i][0]=0.;
      final_output[i][1]=0.;  
    }

    model(final_params,final_input,final_output,1,data_ptr->input_mean,
	  rescar_ptr,files.RoutOutFile);
    //Output2(final_input,final_output,rescar_ptr);

    printf("\nMOSCEM completed in %10.4g SECONDS. Free meories.\n", (time(NULL)-t_tot)*1.);
    printf("-----------------------------------------------\n");

    FreeIntVector(moscem.ObjOptFlag);
    FreeDoubleVector(parameter_ptr->ParDefault);
    FreeDoubleVector(parameter_ptr->UpperBound);
    FreeDoubleVector(parameter_ptr->LowerBound);
    FreeIntVector(parameter_ptr->OptFlag);
    FreeIntVector(parameter_ptr->ParNumber);
    FreeCharMatrix(parameter_ptr->ParName, moscem.nPars);
    FreeCharMatrix(parameter_ptr->ParLongName, moscem.nPars);
    if( parameter_ptr != NULL ) free(parameter_ptr);
 
    FreeIntVector(data_ptr->RankIdx);
    FreeDoubleMatrix(data_ptr->InputData,moscem.nTSteps_Inputs);
    FreeDoubleMatrix(data_ptr->ValData,  moscem.nTSteps_Inputs);
    FreeDoubleMatrix(data_ptr->Output,   moscem.nTSteps_Inputs);

    FreeDoubleMatrix(data_ptr->ParValue, moscem.nSamples);
    FreeDoubleMatrix(data_ptr->ObjValue, moscem.nSamples);
    FreeDoubleVector(data_ptr->ProbValue);
    FreeDouble3Dim(data_ptr->Complex,    moscem.nComplex, npts);
    if ( data_ptr != NULL) free(data_ptr);

    FreeSeqList(moscem.nComplex);  
    FreeCvgList();

    FreeDoubleVector(xpar);
    FreeDoubleVector(prob1);

    return 0;
}

