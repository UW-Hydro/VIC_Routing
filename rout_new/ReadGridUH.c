#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"

/*********************************/
/* Reads the grid uh             */
/*********************************/
void ReadGridUH(char *filename, 
		float **UH_BOX,
		int number_of_cells,
		int **CATCHMENT)
{
  FILE *fp;
  int i,j;

  for(i=1;i<=number_of_cells;i++) { 
    if(CATCHMENT[i][2]==2) { //i.e. routed flow exists 
      UH_BOX[i][1]=1.;
      for(j=2;j<=12;j++) 
	UH_BOX[i][j]=0.;
    }
    else {
      if((fp = fopen(filename, "r")) == NULL) {
	printf("Cannot open %s\n",filename);
	exit(1);
      }
      for(j=1;j<=12;j++) 
	fscanf(fp,"%*f %f",&UH_BOX[i][j]); 
      fclose(fp);
    }
  }

}
