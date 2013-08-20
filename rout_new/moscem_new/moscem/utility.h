/* ============================================
prototypes of utility functions described in 
utility.c, Yuqiong Liu, MARCH  2003
==============================================*/

#ifndef _UTILITY_H_
#define _UTILITY_H_

int    PrintError(char* );
FILE   *Fopen(char*, char*); 
int    *IntVector(int);
double *DoubleVector(int);
int    **IntMatrix(int, int);
double **DoubleMatrix(int, int);
char   **CharMatrix(int, int);
double ***Double3Dim(int, int, int);
int    FreeDoubleVector(double*);
int    FreeIntVector(int*);
int    FreeDoubleMatrix(double**, int);
int    FreeIntMatrix(int**, int);
int    FreeCharMatrix(char**, int);
int    FreeDouble3Dim(double ***, int, int);
void   Mean(int m, int n, double **v, double *ave);
double Min(int n, double *v);
double Max(int n, double *v);
double Round(double);
double Probks(double);
void   Var(int m, int n, double **v, double *var);
#endif
