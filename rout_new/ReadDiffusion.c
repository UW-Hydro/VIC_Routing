#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"

/*********************************/
/* Reads the diffusion file      */
/*********************************/
void ReadDiffusion(char *filename, 
                   ARC **BASIN,
		   int nrows,
		   int ncols)
{
  FILE *fp;
  char dummy[25];
  int i,j;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }
  else printf("File opened %s\n",filename);

  for(i=0;i<6;i++)
    fscanf(fp,"%s %s", dummy, dummy); 

  for(i=nrows;i>=1;i--) { //kept bounds
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%f",&BASIN[i][j].diffusion); 
      if(BASIN[i][j].diffusion<0.) BASIN[i][j].diffusion=0.; 
    }
  }
  fclose(fp);
}
