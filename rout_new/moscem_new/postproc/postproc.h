/* =================================================
function prototypes
Yuqiong Liu, March 2003
==================================================*/

#ifndef _POSTPROC_H_
#define _POSTPROC_H_

#define C        1
#define FORTRAN  2

#define RMSE     1
#define STD      2
#define HMLE     3
#define NSE      4
#define NSC      5
#define BIAS     6
#define WAT      7

void   DriveModel(Moscem *moscem, double *xpar, Data_ptr data_ptr);
void   Initialize(Moscem* moscem, Files* files);
void   LoadData(char *finput, char *fval, Moscem *moscem, Data_ptr data_ptr);
void   LoadPardef(Moscem *moscem, Files *files,double *xpar);
void   LoadParopt(Moscem *moscem, Files *files,double *xpar,double *obj);
void   Output(Moscem *moscem, Data_ptr data_ptr, Files *files);
void   ObjFunc(Moscem *moscem,double **Output,double **ValData,double *Obj,double mean_streamflow);
#if MOD_VER == C  
void   model(double *xpar, double input[NTSTEP1][NINPUT], double out[NTSTEP2][NFLUX]);
#elif MOD_VER == FORTRAN
#define model model_
extern void model(double *xpar, double input[NTSTEP1][NINPUT], double out[NTSTEP2][NFLUX]);
#endif

/* These objective functions are defined in Objectives.c */
double Rmse(double *obs, double *comp,int npts);
double Std(double *obs, double *comp,int npts);
double Hmle(double *obs, double *comp, int npts);
double Nse(double *obs, double *comp, int npts); 
double Nsc(double *obs, double *comp, int npts);
double Bias(double *obs, double *comp, int npts);


#endif
