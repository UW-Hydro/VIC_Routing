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
    int nOptPar;         /* number of parameters for optimizations */
    int nOptObj;         /* number of fluxes for optimization */
    int *ObjOptFlag;     /* flux optimization indices */
    int nSamples;        /* number of points */
    int nTSteps_Inputs;  /* time steps of input data*/
    int nTSteps_Fluxes;  /* time steps of calibration data*/
    int nComplex;        /* number of complexes */
    int nMaxDraw;        /* maximum number of function evaluations */
    int *nValTsteps;     /* valid time steps of calibration data */

} Moscem;

typedef struct Files  {  /* input and output file names */
    char ParControlFile[MAX_FNAME_LEN];  /* parameter defaults, boundaries */
    char ObjControlFile[MAX_FNAME_LEN];  /* flux optimization flags */
    char InputFile[MAX_FNAME_LEN];       /* input (forcing) data */
    char ValFile[MAX_FNAME_LEN];         /* observational data */
    char ObjOutFile[MAX_FNAME_LEN];      /* final objective function values */
    char ParOutFile[MAX_FNAME_LEN];      /* final parameter values */
    char CvgOutFile[MAX_FNAME_LEN];      /* convergence values */
    char RoutOutFile[MAX_FNAME_LEN];     /* output file read by routing program */
    char ResCarFile[MAX_FNAME_LEN];      /* file holding reservoir characteristics*/
} Files;

typedef struct Parameter {  /* parameter info */

    int    *ParNumber;      /* the number of parameters */
    double *ParDefault;     /* parameter default values */
    double *UpperBound;     /* parameter upper bounds */
    double *LowerBound;     /* parameter lower bounds */
    int    *OptFlag;        /* parameter optimization flag */
    char   **ParName;       /* parameter name */
    char   **ParLongName;   /* parameter long name */

} *Parameter_ptr;

typedef struct Data {    
    double **InputData;  /* input data */  
    double **ValData;    /* calibration data */
    double **Output;     /* simulation by the model */
    double **OutputHydro; /* simulation by the model, purpose: hydropower */
    double **OutputFlood; /* simulation by the model, purpose: flood */
    double **OutputMean; /* simulation by the model, purpose: close to mean */
    double **ParValue;   /* parameter values of all the points */
    double **ObjValue;   /* objective function values of all the points */
    double *ProbValue;   /* fintness of all points */
    int    *RankIdx;     /* pareto rank of points */
    double ***Complex;   /* complexes */
    int    Iter;         /* number of function evaluations */
    int    ndom;         /* number of nondominated points */
    double input_mean;   /* mean of input values. ingjerd */
} *Data_ptr;

typedef struct SeqList {  /* sequences of initial points */
    int    Index;        /* Index of nodes */
    double *ParValue;    /* parameter values of initial points */
    double *ObjValue;    /* objective function values of inital points */
    double ProbValue;    /* fitness value of initial points */
    struct SeqList *Next; /* pointer to the next node */ 
} *SeqList_ptr, SeqList;

typedef struct CvgList {   /* List of convergences */
    int Index;            /* index of nodes */
    int Iter;             /* Number of Iterations */
    double *Convergence;  /* convergences of parameters */
    struct CvgList *Next; /* pointer to the next node */
} *CvgList_ptr, CvgList;

typedef struct ResCar  {  /* Reservoir characteristics */
  char Name[50];        /* Name of dam/reservoir */
  int Type;             /* Purpose of reservoir */
  int ObjectiveFunction; /* Objective function to be used in moscem. Overwrites what is said in config.in. 
			    Numbers correspond to those in datatype.h */
  float MaxHead;        /* Maximum head (m) */
  float MinHead;        /* Minimum head (m) */
  float MaxStorage;     /* Max storage (1000 m3) */
  float StartStorage;   /* Start storage (1000 m3) */
  float EndStorage;     /* Min storage (1000 m3) */
  float SurfArea;       /* Surface area (km2) */
  float InstCap;        /* Installed capacity (MW) */
  float MeanFlood;      /* Mean annual flood (naturalized flow) */
} *ResCar_ptr, ResCar;

#endif
