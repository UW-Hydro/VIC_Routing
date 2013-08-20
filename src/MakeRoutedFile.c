#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"

/*********************************/
/* MakeRoutedFile                */
/*********************************/
void MakeRoutedFile(ARC **BASIN,
		    int **CATCHMENT,
		    int nrows,
		    int ncols,
		    int number_of_cells)
{
  FILE *fp;
  int i,j;

  if((fp = fopen("routedcells.txt", "w")) == NULL) {
    printf("Cannot open routedcells.txt\n");
    exit(1);
  }
  else printf("File opened:routedcells.txt\n");

  for(i=0;i<6;i++)
    fprintf(fp,"dummy 1\n"); 

  for(i=nrows;i>=1;i--) { //kept bounds
    for(j=1;j<=ncols;j++) { 
      fprintf(fp,"%d ",BASIN[i][j].routed); 
    }
   fprintf(fp,"\n"); 
  }
  fclose(fp);
}
