#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"
/*********************************/
/* Reads the station file        */
/*********************************/
void ReadStation(char *filename,
		 ARC **BASIN,
		 LIST *STATION,
		 int nrows,
		 int ncols,
		 char uhstring[20][20],
		 int *number_of_stations)
{
  FILE *fp;
  int i,j,irow,icol;
  int already_routed;

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }

  i=1;
  while(!feof(fp)) {
    fscanf(fp,"%d %d %s %d %d %f %d",
	   &STATION[i].id,&already_routed,
           STATION[i].name,&STATION[i].col,
	   &STATION[i].row,&STATION[i].area,&STATION[i].type); 
    fscanf(fp,"%s",uhstring[i]);
    printf("Station name: %s rout now:%d routed before:%d row:%d col:%d type:%d uhstring %s\n",
	   STATION[i].name,STATION[i].id,already_routed,STATION[i].row,
	   STATION[i].col,STATION[i].type,uhstring[i]);
    if(STATION[i].id==1 && already_routed==1) 
      STATION[i].id=0;
    if(already_routed==1) {
        printf("Station already routed: %s %d %d\n",
                STATION[i].name,STATION[i].row,STATION[i].col);
	SearchRouted(BASIN,STATION[i].row,STATION[i].col,nrows,ncols);
    }
    irow=STATION[i].row;
    icol=STATION[i].col;
    if(BASIN[irow][icol].resloc==1 && STATION[i].type==1) {
      STATION[i].type=2;
    printf("Station name, type 2: %s rout now:%d row:%d col:%d type:%d\n",
	   STATION[i].name,STATION[i].id,STATION[i].row,
	   STATION[i].col,STATION[i].type);
    }
    if(STATION[i].col==0) i-=1;
    i+=1;	
  }

  (*number_of_stations)=i-1;
  fclose(fp);
}
