/* =================================================
function prototypes
Yuqiong Liu, March  2003
==================================================*/

#ifndef _MOSCEM_H_
#define _MOSCEM_H_

#define C        1
#define FORTRAN  2
#define MULTI    1
#define SINGL    0

#define RMSE     1
#define STD      2
#define HMLE     3
#define NSE      4
#define POW      5
#define IRR      6
#define FLOOD    7
#define POWFLO   8
#define WAT      9

void   AddSeqList(int i, SeqList_ptr NewPoint);
void   AddCvgList(CvgList_ptr NewPoint);
int    CheckPars(Moscem *moscem, Parameter_ptr par,double *OldPar, double *NewPar);
void   CompObj(Moscem *moscem, Data_ptr data_ptr, Parameter_ptr par_ptr, ResCar_ptr rescar_ptr, Files *files);
void   CompProb(int nobj,int *optIdx,int ns, double **obj,int *rank, double *prob);
void   CompleteParsets(Moscem *moscem, Parameter_ptr par_ptr, double *xpar, double *xpar1);
void   CovMatrix(int m, int n, double **x, double **cov);
int    Convergence(Moscem *moscem, int iter);
void   Gelman(int npar, int nc, double *converg);
void   DriveModel(Moscem *moscem, double *xpar,Data_ptr data_ptr,ResCar_ptr rescar_ptr,Files *files);
void   InitSequence(Moscem *moscem, Data_ptr data_ptr);
void   Latin(Moscem *moscem, Parameter_ptr par_ptr, Data_ptr data);
void   LoadData(char *finput, char *fval, Moscem *moscem, Data_ptr data_ptr);
void   FreeSeqList(int n); 
void   FreeCvgList();
void   GetObjOptFlag(char* fname, Moscem *moscem);
void   model(double *xpar, double input[NTSTEP1][NINPUT], double out[NTSTEP2][NINPUT],int flag,
	     float mean_streamflow, ResCar_ptr rescar_ptr,char outfilename[MAX_FNAME_LEN]);
void   MoscemInit(Moscem* moscem, Files* files);
double NormRand(int* idum);
void   ObjFunc(Moscem *moscem,double **Output,double **ValData,double *Obj, int *iter, 
	       double mean_streamflow,ResCar_ptr rescar_ptr);
#if MOD_ALGO == MULTI
void   OffMetro_multi(Moscem *moscem, Parameter_ptr parameter_ptr, Data_ptr data_ptr,ResCar_ptr rescar_ptr, int iComplex, Files *files);
#elif MOD_ALGO == SINGL
void   OffMetro_singl(Moscem *moscem, Parameter_ptr parameter_ptr, Data_ptr data_ptr,ResCar_ptr rescar_ptr, int iComplex, Files *files);
#endif
void   Output(Moscem *moscem, Data_ptr data_ptr, Parameter_ptr par_ptr, Files *files, double *final_params);
void   Output2(double final_input[NTSTEP1][NINPUT],double final_output[NTSTEP1][NFLUX],ResCar_ptr rescar_ptr);
void   ParameterInit(char* fname, char* fname2, Moscem *moscem, Parameter_ptr parameter_ptr, ResCar_ptr rescar_ptr);
int    ParControl(Moscem *moscem, Parameter_ptr parameter_ptr, double *newPar);
void   Pareto(int m, int n, double** xf, int* idx, int* ndom);
void   ParetoRanking(Moscem *moscem, int npts, double **obj, int *rank, int *ndom1);
void   PartSample(Moscem *moscem, Data_ptr data_ptr);
void   Reshuffle(Moscem *moscem, Data_ptr data_ptr);
void   SortProb(int m, int n, double **data, double *prob);
void   Sqrtm(double **x, double **y, int n);
double UnifRand(int* idum);

/* These objective functions are defined in Objectives.c */
double Rmse(double *obs, double *comp,int npts);
double Std(double *obs, double *comp,int npts);
double Hmle(double *obs, double *comp, int npts);
double Nse(double *obs, double *comp, int npts);
double Pow(double *obs, double *comp, int npts);
double Irr(double *obs, double *comp,int npts);
double Flood(double *obs, double *comp, int npts);
double Wat(double *obs, double *comp, int npts);

extern SeqList_ptr *SeqHead, *SeqTail;
extern CvgList_ptr CvgHead, CvgTail;
extern int seed;

#endif
