#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"

float Find7Q10(int ndays,
	       int nyears,
	       char *name,
	       int basin_number,
	       char *naturalpath,
               float latitude,
               float longitude)

{
  FILE *fp;
  int i,j,month,year,day;
  int count;
  int leap_year;
  float meanflow;
  float mean7q10;
  float stdev7q10;
  float minflow;
  float nestminflow;
  float alfa;
  float beta;
  float *DAY7FLOW;
  float *streamflow;
  char filename[50];

  sprintf(filename,"%sstreamflow_%.4f_%.4f",naturalpath,latitude,longitude);
  if((fp=fopen(filename,"r")) == NULL ){
   printf("Cannot open %s (Find7Q10)\n",filename);
   exit(1);
  }
  else printf("File opened, naturalized flow information (Find7Q10): %s\n",filename);

  DAY7FLOW=(float*)calloc(nyears+1,sizeof(float));
  streamflow=(float*)calloc(8,sizeof(float));

  /* Skip first year, minus 7 days, of output (spinup purposes) */
  for(day=0;day<358;day++) 
    fscanf(fp,"%*f %*f %*f %*f");

  for(year=0;year<nyears+1;year++)
    DAY7FLOW[year]=1e7; //huge number
  i=0;
  meanflow=0;
  year=0;

/* Find 7-day mean flow for the week before new year's */
  for(day=0;day<7;day++) {
    i+=1;
    fscanf(fp,"%*f %*f %*f %f",&streamflow[i]);
    meanflow+=streamflow[i]/7.;
  }

  /* Find 7-day mean flows for the remaining period, 
     and store the minimum 7q10 for each year */
  mean7q10=0.;
  for(year=0;year<nyears;year++) {
    leap_year=IsLeapYear(year+1980);
    for(day=0;day<365+leap_year;day++) {
      i+=1;
      if(i==8) i=1;
      meanflow-=streamflow[i]/7.;
      fscanf(fp,"%*f %*f %*f %f",&streamflow[i]);
      meanflow+=streamflow[i]/7.;
      if(DAY7FLOW[year]>meanflow) DAY7FLOW[year]=meanflow;
    }
    mean7q10+=DAY7FLOW[year]/nyears;
  }

  /* Find stdev of 7q10, alfa and beta for GEV.
   Calculate 7q10 (=minflow) */
  stdev7q10=0.;
  for(year=0;year<nyears;year++) 
    stdev7q10+=(DAY7FLOW[year]-mean7q10)*(DAY7FLOW[year]-mean7q10);
  stdev7q10=sqrt(stdev7q10)/(nyears-1);
  alfa=stdev7q10*sqrt(6)/PI; 
  beta=mean7q10-stdev7q10*0.5772*sqrt(6)/PI;
  minflow=beta-log(-log(0.9))*alfa;

  free(DAY7FLOW);
  free(streamflow);

  return minflow*CONV_M3S_CM;
}

