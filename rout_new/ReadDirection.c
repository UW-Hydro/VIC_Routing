#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*********************************/
/* Reads the flow direction file */
/*********************************/
void ReadDirection(char *filename, 
		   ARC **BASIN,
		   int nrows,
		   int ncols,
		   int *active_cells,
                   int missing)
{
  FILE *fp;
  int i,j;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  for(i=0;i<6;i++)
    fscanf(fp,"%*s %*s"); 

  (*active_cells)=0;

  for(i=nrows;i>=1;i--) { // kept bounds 
    for(j=1;j<=ncols;j++) { 
      fscanf(fp,"%d",&BASIN[i][j].direction);
      BASIN[i][j].flag=0;
      if(BASIN[i][j].direction>missing) 
	(*active_cells)+=1;
    }
  }

  fclose(fp);

  for(i=1;i<=nrows;i++) {
    for(j=1;j<=ncols;j++) {
      if(BASIN[i][j].direction==0 || BASIN[i][j].direction==missing ) {
	BASIN[i][j].tocol=0;
	BASIN[i][j].torow=0;
      } 
      else if(BASIN[i][j].direction==1) {
         BASIN[i][j].tocol=j;
         BASIN[i][j].torow=i+1;
      } 
      else if(BASIN[i][j].direction==2) {
         BASIN[i][j].tocol=j+1;
         BASIN[i][j].torow=i+1;
      } 
      else if(BASIN[i][j].direction==3) {
         BASIN[i][j].tocol=j+1;
         BASIN[i][j].torow=i;
      } 
      else if(BASIN[i][j].direction==4) {
         BASIN[i][j].tocol=j+1;
         BASIN[i][j].torow=i-1;
      } 
      else if(BASIN[i][j].direction==5) {
         BASIN[i][j].tocol=j;
         BASIN[i][j].torow=i-1;
      } 
      else if(BASIN[i][j].direction==6) {
         BASIN[i][j].tocol=j-1;
         BASIN[i][j].torow=i-1;
      } 
      else if(BASIN[i][j].direction==7) {
         BASIN[i][j].tocol=j-1;
         BASIN[i][j].torow=i;
      } 
      else if(BASIN[i][j].direction==8) {
         BASIN[i][j].tocol=j-1;
         BASIN[i][j].torow=i+1;
      } 
    }
  }
}

