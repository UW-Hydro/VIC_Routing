/* ============================================================================
Sqrtm: Calculate matrix square root, calling the fllowing 4 functions:

tred2:  Householder reduction of a real, symmetric matrix to a tridiagonal matix
tqli: QL algorithm to determine the eigenvalues and eigenvectors of a matrix 
      previously reduced by tred2.
MatrixInverse: calculate the inverse of a matrix
MatrixProduct: calculate matrix product

tre2 and tqli are adapted from Numerical Recipe: 1998

Yuqiong Liu   March 2003
===============================================================================*/

#include <math.h>
#include "constant.h"
#include "utility.h"
#include "stdlib.h"

#define SIGN(a,b) ((b)<0 ? -fabs(a) : fabs(a))

void tred2(double **a, int n, double *d, double *e);
void tqli(double *d, double *e, int n, double **z);
void MatrixInverse(double **a, int n, double **b);
void MatrixProduct(double **a, double **b, double **c, int n);

void Sqrtm(double **x, double **y, int n)    {
                    
    double **a, *d, *e, **b, **a1,**diag, **c1, **c2;
    int i,j;
    a = DoubleMatrix(n+1, n+1);
    d = DoubleVector(n+1);
    e = DoubleVector(n+1);
    b = DoubleMatrix(n+1, n+1);
    a1 = DoubleMatrix(n+1, n+1);
    diag = DoubleMatrix(n+1, n+1);
    c1 = DoubleMatrix(n+1, n+1);
    c2 = DoubleMatrix(n+1, n+1);

    for (i=1; i<=n; i++) {
        for(j=1; j<=n; j++) a[i][j] = x[i-1][j-1]; 
    }

/* ================ reduce the matrix "a" to a tridiagonal matrix ================== */
    
    tred2(a, n, d, e);
/*    for (i=1;i<=n;i++) printf("%12.3e %12.3e\n",d[i],e[i]);*/

/* =============== determine the eigenvalues and eigenvectors of "a" =============== */
    tqli(d, e, n, a);

/* ============== determine the inverse of the eigenvector matrix =============== */    
    for (i=1; i<=n; i++)  
        for(j=1; j<=n; j++)  a1[i][j] = a[i][j];
    MatrixInverse(a1, n, b);

/* ============= determine the matrix square root of the original matrix =========*/
    for (i=1; i<=n; i++)    {
       for(j=1; j<=n; j++)
           if(i==j) {
               if (d[i]<=0) diag[i][j] = 0.;
               else diag[i][j] = sqrt(d[i]);
           }
           else    diag[i][j] = 0.;
    }
    MatrixProduct(a, diag, c1, n);
    MatrixProduct(c1, b, c2, n);
    
    for (i=1; i<=n; i++)
        for(j=1; j<=n; j++) y[i-1][j-1]=c2[i][j];

    FreeDoubleMatrix(b,n+1);  
    FreeDoubleMatrix(a1, n+1);
    FreeDoubleMatrix(a,n+1);
    FreeDoubleVector(d);
    FreeDoubleVector(e);
    FreeDoubleMatrix(diag,n+1);
    FreeDoubleMatrix(c1, n+1);
    FreeDoubleMatrix(c2, n+1);
}
        

/* The following two functions are copied from Numerical Recipes 1988-1992 
Householder reduction of a real, symmetric matrix a[1 . . n] [1 . . n]. On output,
a is replaced by the orthogonal matrix Q affecting tje transformation. d[1 . . n] 
returns the diagonal elements of the triagonal matrix, and e[1 . . n] the off-diagional
elements, with e[1]=0. Several statements, as noted in comments, can be omitted if
only eigenvalues are to be found, in which case a contains no useful information on
output. Otherwise they are to be included.  */ 

void tred2(double **a, int n, double *d, double *e) {

    int l, k, j, i;
    double scale, hh, h, g, f;

    for (i=n; i>=2; i--)  {
        l = i-1;
        h = scale = 0.0;
        if (l>1) {
            for (k=1; k<=l; k++) scale += fabs(a[i][k]);
            if (scale == 0.0) e[i] = a[i][l];     /* skip transformation */
            else   {
                for (k=1; k<=l; k++) {
                    a[i][k] /= scale;    /* use scaled a's for transformation */
                    h += a[i][k]*a[i][k];   /* form sigma in h */
                }
                f = a[i][l];
                g = (f>=0.0 ? -sqrt(h) : sqrt(h));
                e[i] = scale * g;
                h -= f * g;
                a[i][l] = f-g;    /* store a in ith row of a */
                f = 0.0;
                for (j=1; j<=l; j++)   {
                /* next statement can be omitted if eigenvectors not wanted */
                    a[j][i] = a[i][j]/h;   /* store u/H in ith column of a */
                    g=0.0;     /* form an element of A.u in g */
                    for (k=1; k<=j; k++) 
                        g += a[j][k]*a[i][k];
                    for (k=j+1; k<=l; k++)
                        g += a[k][j]*a[i][k];
                    e[j] = g/h;       /* form element of p in temporarily unused element of e */
                    f += e[j]*a[i][j];
                }
                hh = f/(h+h);   /* form K, */
                for (j=1; j<=l; j++)  {   /* form q and store in e overwriting p */
                    f = a[i][j];
                    e[j] = g = e[j]-hh*f;
                    for (k=1; k<=j; k++)   /* reduce a */
                        a[j][k] -= (f*e[k]+g*a[i][k]);
                    
                }
            }  
        }
        else 
            e[i] = a[i][l];
        d[i] = h;
         
    }   
    /* next statement can be omitted if eigenvectors not wanted */
    d[1] = 0.0;
    e[1] = 0.0;
    /* contents of this loop can be omitted if eigenvectors not wanted except 
       for statement d[i] = a[i][i]; */
    for (i=1; i<=n; i++)  {   /* begin accumulation of transformation matrices */
        l=i-1;
        if (d[i]) {          /* this block skipped when i=1 */
            for (j=1; j<=l; j++)   {   
                g = 0.0;
                for (k=1; k<=l; k++)   /* use u and u/H stored in a to form P.Q */
                    g += a[i][k] * a[k][j];
                for (k=1; k<=l; k++)
                    a[k][j] -= g*a[k][i];
            }
        }
        d[i] = a[i][i];   /* this statement remains */
        a[i][i] = 1.0;    /* reset row and column of a to identity matrix for next iteration */
        for (j=1; j<=l; j++) a[j][i] = a[i][j]=0.0;

    }

}

/* QL algorithm with implicit shifts, to determine the eigenvalues and eigenvectors of a
real, symetric, trigiagonal matrix, or of a real, symmetric matrix previously reduced by
tred2. On input, d[1 . .n] contains the diagonal elements of the tridiagonal matrix. On 
output, it returns the eigenvalues. The vector e[1 . . n] inputs the subdiagional elements
of the tridiagonal matrix, with e[1] arbitrary. On output e is destroyed. When finding only
the eigenvalues, several lines may be omitted, as noted in the comments. If the eigenvectors
of a tridiagonal matrix are desired, the matrix z[1 . . n][1 . . n] is input as the identity
matrix. If the eigenvectors of a matrix that has been reduced by tred2 are required, then z
is input as the matrix output by tred2. In either case, the kth column of z returns the nor-
malized eigenvector corresponding to d[k].  */

void tqli(double *d, double *e, int n, double **z) {

    double s, r, p, g, f, dd, c, b;
    int m, l, iter, i, k;

    for (i=2; i<=n; i++) e[i-1] = e[i];  /* convenient to renumber the elements of e */
    e[n] = 0.0;
    for (l=1; l<=n; l++)   {
        iter = 0;
        do {
            for (m=l; m<=n-1; m++)  {/* look for a single small subdiagonal element to split the 
                                     matrix*/ 
                dd = fabs(d[m])+fabs(d[m+1]);
                if (fabs(e[m]+dd) == dd) break;
            }
            if (m!=l)  {
               if (iter++ == 100) { printf("Too many iterations in tqli\n"); exit(1); }
               g = (d[l+1]-d[l])/(2.0*e[l]);   /* form shift */
               r = sqrt((g*g)+1.0);
               g = d[m]-d[l]+e[l]/(g+SIGN(r,g));   /*this is dm - ks */
               s=c=1.0;
               p=0.0;
               for (i=m-1; i>=l; i--) {  /* a plane rotation in the original QL, followed by */
                   f=s*e[i];             /* Givens rotations to restore tridigonal form */ 
                   b=c*e[i];
                   if (fabs(f) >= fabs(g)) {
                       c = g/f;
                       r = sqrt(c*c+1.0);
                       e[i+1] = f*r;
                       c *= (s=1.0/r);
                   } else {
                       s=f/g;
                       r = sqrt(s*s+1.0);
                       e[i+1] = g * r;
                       s *= (c=1.0/r);
                   } 
                   g=d[i+1]-p;
                   r=(d[i]-g)*s+2.0*c*b;
                   d[i+1] = g+(p=s*r);
                   g=c*r-b;
                   /* next loop can be omitted if eigenvectors not wanted */
                   for (k=1; k<=n; k++) {
                       f=z[k][i+1];            /*form eigenvectors */
                       z[k][i+1] = s*z[k][i]+c*f;
                       z[k][i] = c*z[k][i]-s*f;
                   }
               }
               d[l] -= p;
               e[l] = g;
               e[m] = 0.0;
           }
       }while (m!=l);

    } 
}


void MatrixInverse(double **a, int n, double **b)  {

    int i, j, k;
    double temp;

    for (i=1; i<=n; i++)   {
       for (j=1; j<=n; j++)
           if (i==j) b[i][j] = 1.;
           else b[i][j] = 0.;
    }

    for (i=1; i<=n; i++)  {
        if (a[i][i] == 0.0) {
            for (j=i+1; j<=n; j++)  {
                if (a[j][i] != 0.0)  {
                   for (k=1; k<=n; k++) {
                       temp = a[i][k]; a[i][k] = a[j][k]; a[j][k] = temp;
                       temp = b[i][k]; b[i][k] = b[j][k]; b[j][k] = temp;
                   }
                   break;
                }
            }
        }

        if (a[i][i] !=1 && a[i][i]!=0) {
            temp = a[i][i];
            for (j=i; j<=n; j++)  a[i][j] = a[i][j] / temp;             
            for (j=1; j<=n; j++)  b[i][j] = b[i][j] / temp;
        }

        for (j=1; j<=n; j++)
           if (j!=i && a[j][i] != 0.)  {
               temp = a[j][i];
               for(k=i; k<=n; k++)   a[j][k] = a[j][k]-a[i][k]*temp;
               for(k=1; k<=n; k++)   b[j][k] = b[j][k]-b[i][k]*temp;
           }
     }

}                                           

void MatrixProduct(double **a, double **b, double **c, int n)   {

     int i,j,k;
     
     for (i=1;i<=n; i++)  {
         for(j=1; j<=n; j++)  {
             c[i][j]=0.;
             for (k=1; k<=n;k++) c[i][j] = c[i][j]+a[i][k]*b[k][j];
         }
     }
}


