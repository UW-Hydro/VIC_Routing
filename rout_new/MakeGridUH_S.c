#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "rout.h"

/***********************************************************/
/* MakeGridUH_S                                            */
/***********************************************************/
void MakeGridUH_S(ARC **BASIN,
		  int **CATCHMENT,
		  LIST *STATION,
		  int number_of_cells,
		  int cellnumber,
		  float **UH_DAILY,
		  float ***UH,
		  float **FR,
		  float **UH_BOX,  //[number_of_cells][12]
		  float **UH_S,    //[number_of_cells][108]
		  char *uh_string)

{
  FILE *fp;
  float sum;
  int i,j,k,l,n,t,u,ii,jj,tt;
  char none[5]="NONE";
  char name[10];

  if(strcmp(uh_string,none) !=0) {
    printf("Reading UH_S grid from file\n");
    if((fp = fopen(uh_string, "r")) == NULL) {
      printf("Cannot open uh_file %s\n",uh_string);
      exit(1);
    }
    else printf("File opened for reading: %s\n",uh_string);
    for(n=1;n<=number_of_cells;n++) 
      for(k=1;k<KE+UH_DAY;k++) 
	fscanf(fp,"%f ",&UH_S[n][k]);
  }
  else {
    printf("Making UH_S grid.... It takes a while...\n");
    strcpy(name,STATION[cellnumber].name);
    strcat(name,".uh_s");
    if((fp = fopen(name, "w")) == NULL) {
      printf("Cannot open %s\n",name);
      exit(1);
    }
    else printf("File opened for writing: %s\n",name);

    for(n=1;n<=number_of_cells;n++) {
      printf("grid cell %d out of %d, row %d col %d\n",
	     n,number_of_cells,CATCHMENT[n][0],CATCHMENT[n][1]);
      for(k=1;k<=UH_DAY;k++)
        UH_DAILY[n][k] = 0.0;
        i=CATCHMENT[n][0]; //row
        j=CATCHMENT[n][1]; //col
      for(t=1;t<=24;t++) {
	FR[t][0]=1.0/24.;
	FR[t][1]=0.0;
      }
      for(t=25;t<=TMAX;t++) {
	FR[t][0]=0.0;
	FR[t][1]=0.0;
      }

    loop:
      if((i!=STATION[cellnumber].row || j!=STATION[cellnumber].col) ||
	 (STATION[cellnumber].type==3 && CATCHMENT[cellnumber][2]==2)) { 
	for(t=1;t<=TMAX;t++) {
	  for(l=1;l<=LE;l++) {
	    if((t-l)>0) 
	      FR[t][1]=FR[t][1]+FR[t-l][0]*UH[i][j][l];
	  }
	}
	ii=BASIN[i][j].torow;
	jj=BASIN[i][j].tocol;
	i=ii;
	j=jj;
	for(t=1;t<=TMAX;t++) {
	  FR[t][0]=FR[t][1];
	  FR[t][1]=0.0;
	}
      }
      if((i!=STATION[cellnumber].row || j!=STATION[cellnumber].col) &&
	 STATION[cellnumber].type!=3) {
	goto loop;
      }

      for(t=1;t<=TMAX;t++) {
	tt=(t+23)/24;
	UH_DAILY[n][tt] += FR[t][0];
      }
    } // number_of_cells

    for(n=0;n<=number_of_cells;n++) {
      for(k=0;k<KE+UH_DAY;k++) {
	UH_S[n][k]=0.;
      }
    }

    for(n=1;n<=number_of_cells;n++) {
      for(k=1;k<=KE;k++) {
	for(u=1;u<=UH_DAY;u++) 
	  UH_S[n][k+u-1] = UH_S[n][k+u-1]+UH_BOX[n][k]*UH_DAILY[n][u];
      }	
      sum=0.0;
      for(k=1;k<KE+UH_DAY;k++) {
	sum+=UH_S[n][k];
      }
      for(k=1;k<KE+UH_DAY;k++)
	UH_S[n][k]=UH_S[n][k]/sum;
    }

    for(n=1;n<=number_of_cells;n++) {
      for(k=1;k<KE+UH_DAY;k++) {
	if(UH_S[n][k]>0) fprintf(fp,"%f ",UH_S[n][k]);
	else fprintf(fp,"%.1f ",UH_S[n][k]);
      }
      fprintf(fp,"\n");
    }
  }
}
