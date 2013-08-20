#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*********************************/
/* MakeDirectionFile             */
/*********************************/
void MakeDirectionFile(ARC **BASIN,
		       int **CATCHMENT,
		       int nrows,
		       int ncols,
		       int number_of_cells,
		       float xllcorner,
		       float yllcorner,
		       float size,
		       int missing)
{
  FILE *fp;
  int i,j;

  if((fp = fopen("dirtest.txt", "w")) == NULL) {
    printf("Cannot open dirtest.txt\n");
    exit(1);
  }
  else printf("File opened:dirtest.txt\n");

  fprintf(fp,"ncols\t %d\n",ncols);
  fprintf(fp,"nrows\t %d\n",nrows);
  fprintf(fp,"xllcorner\t %.4f\n",xllcorner);
  fprintf(fp,"yllcorner\t %.4f\n",yllcorner);
  fprintf(fp,"cellsize\t %.4f\n",size);
  fprintf(fp,"NODATA_value\t %d\n",missing);

  for(i=nrows;i>=1;i--) { //kept bounds
    for(j=1;j<=ncols;j++) { 
      fprintf(fp,"%d ",BASIN[i][j].direction); 
    }
   fprintf(fp,"\n"); 
  }
  fclose(fp);
}

