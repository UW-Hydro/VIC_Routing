/* ==========================================================
Returns a gaussian N(0,1) random variable, modified from 
Numerical Recipe, 1988

Yuqiong Liu, March 2003
============================================================*/

#include <stdlib.h>
#include <math.h>
#include "constant.h"
#include "datatype.h"
#include "moscem.h"

double NormRand(int *idum)  {

     static int iset=0;
     static double gset;
     double fac,r,v1,v2;

     if (*idum <0) iset=0;
     if  (iset == 0) {
	 do {
	     v1=2.0 * UnifRand(idum)-1.0;
	     v2=2.0 * UnifRand(idum)-1.0;
	     r=v1*v1+v2*v2;
	 } while (r >= 1.0 || r == 0.0);
	     fac=sqrt(-2.0*log(r)/r);
	     gset=v1*fac;
	     iset=1;
	     return v2*fac;
     } 
     else {
	 iset=0;
	 return gset;
     }
}
