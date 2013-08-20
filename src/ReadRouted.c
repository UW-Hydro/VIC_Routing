#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*********************************/
/* Reads the routed cells     */
/*********************************/
void ReadRouted(char *filename, 
		ARC **BASIN,
		int nrows,
		int ncols)

{
  FILE *fp;
  char dummy[25];
  float fdummy;
  int i,j;
  int ivalue;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open routed file %s, setting values to zero\n",filename);
   for(i=1;i<=nrows;i++)  
      for(j=1;j<=ncols;j++)  
	BASIN[i][j].routed=0;
  }

  else {
    printf("Routing information file opened %s\n",filename);
    for(i=0;i<6;i++)
      fscanf(fp,"%s %f", dummy, &fdummy); 
    for(i=nrows;i>=1;i--) { 
      for(j=1;j<=ncols;j++) { 
	fscanf(fp,"%d",&ivalue); 
	if(ivalue<EPS)
	  BASIN[i][j].routed=0;
	else BASIN[i][j].routed=ivalue;
      }
    }
    fclose(fp);
  }
}
