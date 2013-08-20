/* ============== Utility.c ======================
   Some utility functions used in this program

Yuqiong Liu, March 2003
================================================= */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "constant.h"
#include "utility.h"

int PrintError(char* error_text)
{
    fprintf(stderr,"%s\n",error_text);
    fprintf(stderr,"...now exiting to system...\n");
    exit(1);
    return 1;
}

FILE* Fopen(char* fname, char* mode) 
{
    FILE* fp;
    char str[MAX_LINE_LEN];

    if ((fp=(fopen(fname, mode))) == NULL) {
	sprintf(str,"%s %s\n","Can not open file:",fname);
	PrintError(str);
    }
    return fp;
}


double *DoubleVector(int n)
{
    double *v;
   
    v = (double *) calloc((size_t) n, (size_t) sizeof(double));
    if (!v) PrintError("allocation failure in DoubleVector()");
    return v;
}

double **DoubleMatrix(int m, int n)
{
   register int i;
   double **x;

   x=(double **) calloc((size_t) m, (size_t) sizeof(double *));
   if (!x) PrintError("allocation failure 1 in DoubleMatrix()");
   for(i=0;i<m;i++) {
      x[i]=(double *) calloc((size_t) n, (size_t) sizeof(double));
      if (!x[i]) PrintError("allocation failure 2 in DoubleMatrix()");
   }
   return x;
}

int FreeDoubleVector(double *v)
{
   free((char *) v);
   return 0;
}

int FreeDoubleMatrix (double **x, int m)
{
   register int i;

   for (i=0;i<m;i++) free((char *) x[i]);
   free((char *) x);
   return 0;
}

