#ifndef _MODEL_SAC_H
#define _MODEL_SAC_H
/* THIS IS THE HEADER FILE WHERE SPECIFIC HEADER FILES
 * PROTOTYPES, STRUCTURES, AND FUNCTIONS ARE DEFINED */


/*  PROTOTYPE DEFINITIONS */
#define MAXTSTEP 8000  /*4081 13214 */ /* 4081 */ /* Maximum numberoftimesteps */
#define MAXPARAMS 5  /* Maximum number of output parameters from model */
#define MAXGAGE 1 /* Maximum number of gages */
#define TIMESPERDAY 2
#define CONV_M3S_CM 86400.
#define MAX_FNAME_LEN  50 /* same as in constant.h */

/* STRUCTURE DEFINITIONS */
struct OBS {
  int Precipperday;
  int Qobsperday;
  int PETperday;
  int datalength;
  int Year [MAXTSTEP];
  int Month [MAXTSTEP];
  int Day [MAXTSTEP];
  double Precip [MAXTSTEP] [TIMESPERDAY];
  double PET [MAXTSTEP] [MAXGAGE];
  double Qin[MAXTSTEP];
  double Qoutmin[MAXTSTEP];
  double WaterDemand[MAXTSTEP];
  double Resevap[MAXTSTEP];
  double MeanFlood[MAXTSTEP];
};

struct OUTPUT {
  int numcomponents;
  int message[MAXTSTEP];
  double Qcomp_total [MAXTSTEP] [MAXPARAMS];
  double Qcomp [MAXTSTEP] [MAXPARAMS];
  double states [MAXTSTEP] [MAXPARAMS];
  double excess [MAXTSTEP] [MAXPARAMS];
  double ET [MAXTSTEP] [MAXPARAMS];
};

typedef struct ResCar  {  /* Reservoir characteristics */
  char Name[50];        /* Name of dam/reservoir */
  int Type;             /* Purpose of reservoir */
  int ObjectiveFunction;
  float MaxHead;        /* Maximum head (m) */
  float MinHead;        /* Minimum head (m) */
  float MaxStorage;     /* Max storage (1000 m3) */
  float StartStorage;   /* Max storage (1000 m3) */  
  float EndStorage;     /* Min storage (1000 m3) */
  float SurfArea;       /* Surface area (km2) */
  float InstCap;        /* Installed capacity (MW) */
  float Demand[12]; 
} *ResCar_ptr, ResCar;


/* FUNCTION DECLARATION */
/*int ReservoirHydro(double *xpar,struct OBS *obs,
		   struct OUTPUT *output,int flag,
		   float mean_streamflow,ResCar_ptr rescar_ptr,
		   float *scalingfactor,char outfilename[MAX_FNAME_LEN]);
int ReservoirFlood(double *xpar,struct OBS *obs,
		   struct OUTPUT *output,int flag,
		   float mean_streamflow,ResCar_ptr rescar_ptr,
		   float *scalingfactor,char outfilename[MAX_FNAME_LEN]);
int ReservoirIrr(double *xpar,struct OBS *obs,
		 struct OUTPUT *output,int flag,
		 float mean_streamflow,ResCar_ptr rescar_ptr,
		 float *scalingfactor,char outfilename[MAX_FNAME_LEN]);
int ReservoirWat(double *xpar,struct OBS *obs,
		 struct OUTPUT *output,int flag,
		 float mean_streamflow,ResCar_ptr rescar_ptr,
		 float *scalingfactor,char outfilename[MAX_FNAME_LEN]);*/
int Reservoir(double *xpar,struct OBS *obs,
	      struct OUTPUT *output,int flag,
	      float mean_streamflow,ResCar_ptr rescar_ptr,
	      char outfilename[MAX_FNAME_LEN]);
int DaysOfMonth(int month);
#endif
