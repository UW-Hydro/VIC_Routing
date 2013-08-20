#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*********************************/
/* Reads reservoir locations     */
/*********************************/
void ReadReservoirs(char *filename,
		    int nrows,
		    int ncols,
		    ARC **BASIN)

{
  FILE *fp;
  int i,j,h,year,icol,irow,count;

  count=0;
  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s, setting values to 0\n",filename);
    for(i=1;i<=nrows;i++)  
      for(j=1;j<=ncols;j++)  
	BASIN[i][j].resloc=0;
  }
  else {
    printf("Reservoir information file opened: %s\n",filename);
    while(fscanf(fp,"%d %d %*f %*f %*d %*s %d %*f %*f %*f %*d %*d %*d %d %*s",
		 &icol,&irow,&year,&h)!=EOF) {
      BASIN[irow][icol].resloc=1;
      count+=1;
    }
  fclose(fp);
  }
}
