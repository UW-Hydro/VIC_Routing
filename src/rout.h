#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "rout_def.h"

/*** SubRoutine Prototypes ***/
void CalculateMeanInflow(double *,TIME *,int,float *,
			 float *,float *);
void CalculateNumberDaysMonths(int,int,int,int,
			       int,int,int*,int *,int *);
float Find7Q10(int,int,char *,int,char *,float,float);
void FindRowsCols(char *,int *,int *, float *, 
		   float *,float *,int *); 
void FindStartOfOperationalYear(float *,float,int,char *,char *,
				int *,int *,float,float);
int IsLeapYear(int);
void MakeConvolution(int,int,int,int **,ARC **,
		     double *,double *,double *,float **,
		     LIST *,int,float,float,float,
		     char *,char *,char *,int,TIME *,float *,
		     int,int,int,int);
void MakeDirectionFile(ARC **,int **,int,int,int,float,
		       float,float,int);
void MakeRoutedFile(ARC **,int **,int,int,int);
void MakeGridUH_S(ARC **,int **,LIST *,
		  int,int,float **,float ***,float **,float **,
		  float **,char *);
void MakeUH(float ***UH,ARC **,int,int);
void ReadDataForReservoirEvaporation(char *,float **,float,float,
				     int,int,int,int); 
void ReadDiffusion(char *,ARC **,int,int); 
void ReadDirection(char *,ARC **,int,int,int *,int); 
void ReadFraction(char *,ARC **,int,int); 
void ReadGridUH(char *,float **UH_BOX,int,int **);
void ReadReservoirs(char *,int,int,ARC **);
void ReadRouted(char *, ARC **, int,int); 
void ReadStation(char *,ARC **,LIST *,int,int,
		 char uhstring[20][20],int *);
void ReadVelocity(char *, ARC **,int,int); 
void ReadWaterDemand(char *,char *,float **,int,int,int); 
void ReadXmask(char *, ARC **,int,int); 
void ReservoirRouting(char *,char *,double *,float *,
		      float *,float *,float *,float **,float **,
		      TIME *,char *,int,int,int,int,
		      int,int,int,char *);
void SearchCatchment(ARC **,int **,int,
		     int,int,int,int,int *,int *);
void SearchRouted(ARC **,int,int,int,int);
void WriteData(double *,float *,float *,float *,
	       float *,char *,char *,int,int,TIME *,
	       float,int,int,int,int,int,float,float,int,int);
