#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"

/*********************************/
/* Reads the fraction file       */
/*********************************/
void ReadFraction(char *filename, 
		  ARC **BASIN,
		  int nrows,
		  int ncols)
{
  FILE *fp;
  char dummy[25];
  float fdummy;
  int i,j;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }
  else printf("Fraction file opened %s\n",filename); 

  for(i=0;i<6;i++)
  fscanf(fp,"%s %f", dummy, &fdummy); 

  for(i=nrows;i>=1;i--) { 
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%f",&BASIN[i][j].fraction); 
      if(BASIN[i][j].fraction<fdummy+EPS)
	BASIN[i][j].fraction=0.;
    }
  }
  fclose(fp);
}
