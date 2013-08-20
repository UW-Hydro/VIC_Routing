#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/****************************************************
MakeUH:
 Calculate impulse response function for grid cells
 using equation (15) from Lohmann, et al. (1996)  
 Tellus article
*****************************************************/
void MakeUH(float ***UH,
	    ARC **BASIN,
	    int nrows,
	    int ncols)
{
  int i,j,k;
  float time,exponent;
  float green; // Green's function h(x,t)
  float green_normal; // Normalizing

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      time=0.0;
      for(k=1;k<=LE;k++) {
	time=time+DELTA_T;
	if(BASIN[i][j].velocity>0.0) {
	  exponent=((BASIN[i][j].velocity*time-BASIN[i][j].xmask)*
		    (BASIN[i][j].velocity*time-BASIN[i][j].xmask))/
	    (4.0*BASIN[i][j].diffusion*time);
	  if(exponent>69.0)  //this number is in the fortran version, but what does it mean? 
	    green=0.0;
	  else 
	    green=1.0/(2.0*sqrt(PI*BASIN[i][j].diffusion)) * 
	      BASIN[i][j].xmask/pow(time,1.5) * 
	      exp(-exponent);  // eq. 15 in Tellus article
	}
	else
	  green=0.0;
	UH[i][j][k]=green;
      }
    }
  }

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      green_normal = 0.0;
      for(k=1;k<=LE;k++) 
	green_normal += UH[i][j][k];
      if (green_normal > 0.0) {
	for(k=1;k<=LE;k++) 
	  UH[i][j][k] = UH[i][j][k]/green_normal; //normalizing?
      }
    }
  }
}
