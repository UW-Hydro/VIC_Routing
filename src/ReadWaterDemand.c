#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"

void ReadWaterDemand(char *demandpath,
		     char *resname,
		     float **WATER_DEMAND,
		     int start_year,
		     int stop_year,
		     int ndays)
{
  FILE *fp;
  char filename[150];
  char fmtstr[150];
  int i,j;

  strcpy(filename,demandpath);
  sprintf(fmtstr,"/%s.calc.irrdemand.monthly",resname);
  strcat(filename,fmtstr);

  if((fp = fopen(filename, "r")) == NULL) {
    printf("Cannot open %s\n",filename);
    exit(1);
  }
  else printf("File opened %s\n",filename);

  /* Read water demand. Numbers in file are in m3s-1,
     recalculate to m3 day-1 */
  for(i=0;i<stop_year-start_year+1;i++) {
    for(j=1;j<=12;j++) {
      fscanf(fp,"%*d %*d %*f %f",&WATER_DEMAND[i][j]);
      if(i<1) printf("readwaterdemand %d %d %f m3s-1\t",i,j,WATER_DEMAND[i][j]);
      WATER_DEMAND[i][j]*=CONV_M3S_CM;
      if(i<1) printf("%f m3day-1\n",WATER_DEMAND[i][j]);
    }
  }

  fclose(fp);
}
