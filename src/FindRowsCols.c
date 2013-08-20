#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"

/**********************************************/
/* FindRowsCols - finds rows,cols,xllcorner,
   yllcorner,size                             */
/**********************************************/
void FindRowsCols(char *filename, 
		  int *nrows,
		  int *ncols,
		  float *xllcorner,
		  float *yllcorner,
		  float *size,
		  int *missing)
{
  FILE *fp;
  char dummy[25];

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  fscanf(fp,"%s %d", dummy, &(*ncols)); 
  fscanf(fp,"%s %d", dummy, &(*nrows)); 
  fscanf(fp,"%s %f", dummy, &(*xllcorner));
  fscanf(fp,"%s %f", dummy, &(*yllcorner));
  fscanf(fp,"%s %f", dummy, &(*size)); 
  fscanf(fp,"%s %d", dummy, &(*missing)); 

  fclose(fp);

  if((*nrows)>MAXROWS || (*ncols)>MAXCOLS){
   printf("Incorrect dimensions: Reset nrow and ncol in main to %d, %d\n",
     (*nrows), (*ncols));
   exit(0);
   }
}
