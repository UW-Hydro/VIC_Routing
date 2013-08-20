/* ====================================================
Define the data structures 

Yuqiong Liu, March 2003
=======================================================*/

#ifndef _DATATYPE_H_
#define _DATATYPE_H_

typedef struct Moscem  {  /* moscem parameters */

    int nPars;           /* number of parameters */
    int nInputs;         /* number of input data variables */
    int nFluxes;         /* number of fluxes   */
    int nTSteps_Inputs;  /* time steps of input data*/
    int nTSteps_Fluxes;  /* time steps of calibration data*/
    int nParIdx;         /* index of the parameter set to be used */
    int nSamples;        /* total number of the parameter sets */
    int iMod;            /* what to calculate */
    double *ObjMax;      /* maximum objective values can be accepted, used to filter solutions */
    
} Moscem;

typedef struct Files  {  /* input and output file names */
    char InputFile[MAX_FNAME_LEN];       /* input (forcing) data */
    char ValFile[MAX_FNAME_LEN];         /* observational data */
    char ParFile[MAX_FNAME_LEN];         /* file contents parameter sets */ 
    char ObjFile[MAX_FNAME_LEN];         /* file contents objective function values */
    char ParOptFile[MAX_FNAME_LEN];      /* file contents optimal parameter sets */  
    char TsOutFile[MAX_FNAME_LEN];       /* time series of a given point */
    char ObjOutFile[MAX_FNAME_LEN];      /* objective function values after postproc */
    char TrdoffFile[MAX_FNAME_LEN];      /* Lower and upper boundaries and the average of time series */

} Files;

typedef struct Data {    
    double **InputData;   /* input data */  
    double **ValData;     /* calibration data */
    double **Output;      /* simlation by the model */
    double *ParValue;     /* parameter values*/
    double *ObjValue;     /* objective function values*/
    double **uppBound;    /* upper boundary series */
    double **lowBound;    /* lower boundary time series */
    double **average;     /* average time series */
    double input_mean;
} *Data_ptr;

#endif
