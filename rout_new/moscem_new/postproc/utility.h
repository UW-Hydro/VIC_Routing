/* ============================================
prototypes of utility functions described in 
utility.c, Yuqiong Liu, March  2003
==============================================*/

#ifndef _UTILITY_H_
#define _UTILITY_H_

int    PrintError(char* );
FILE   *Fopen(char*, char*); 
double *DoubleVector(int);
double **DoubleMatrix(int, int);
int    FreeDoubleVector(double*);
int    FreeDoubleMatrix(double**, int);

#endif
