#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "rout.h"
/*******************************************/
/* ReadDataForReservoirEvaporation.c       */
/*******************************************/
void ReadDataForReservoirEvaporation(char *inpath,
				     float **RESEVAPDATA,
				     float lat,
				     float lon,
				     int skip,
				     int nbytes,
				     int ndays,
				     int decimal_places)

{
  FILE  *fin;
  int    i,j,k,l;
  int    dt;
  int    Ncols;
  float  *data, *data2, *sum;
  float  dummy;
  char   *name;
  char   fmtstr[17];
  char   file_name[100];
  char   tmpdata[100];
  int    month;
  int    datacnt;
  int    *startdate, *enddate;
  char   *cptr;
  short int *siptr;
  unsigned short int *usiptr;
  int    *iptr;
  float  *fptr;

  cptr = (char *)malloc(1*sizeof(char));
  name = (char *)malloc(600*sizeof(char));
  siptr = (short int *)malloc(1*sizeof(short int));
  usiptr = (unsigned short int *)malloc(1*sizeof(unsigned short int));
  iptr = (int *)malloc(1*sizeof(int));
  fptr = (float *)malloc(1*sizeof(float));

  /** Create Grid Cell Flux Filename and Open for Reading **/
  strcpy(name,inpath);
  sprintf(fmtstr,"fluxes_%%.%if_%%.%if",decimal_places,decimal_places); // original
  //sprintf(fmtstr,"fluxes_%%.%if_%%.%if",4,4); // hard coded -Tian
  sprintf(file_name,fmtstr,lat,lon);
  strcat(name,file_name);
  puts(name);
  fin  = fopen(name, "rb");
  if (fin == NULL){
    printf("Infile (ReadDataForResevap) does not exist %s\n",name);
    exit(1); 
  }
  else printf("File opened for reading of data needed to calculate reservoir evaporation %s\n",name);

  for(i=0;i<skip;i++) 
    fread(tmpdata,nbytes,sizeof(char),fin);

  for(i=1;i<=ndays;i++) {
    /* year */
    fread(iptr,1,sizeof(int),fin);
    /* month */
    fread(iptr,1,sizeof(int),fin);
    /* day */
    fread(iptr,1,sizeof(int),fin);
    /* prec */
    fread(fptr,1,sizeof(float),fin);
    /* prec_orig */
    fread(fptr,1,sizeof(float),fin);
    /* snowf */
    fread(fptr,1,sizeof(float),fin);
    /* evap */
    fread(fptr,1,sizeof(float),fin);
    RESEVAPDATA[i][0] = fptr[0];
    /* runoff */
    fread(fptr,1,sizeof(float),fin);
    /* baseflow */
    fread(fptr,1,sizeof(float),fin);
    /* swe */
    fread(fptr,1,sizeof(float),fin);
    /* extract_water */
    fread(fptr,1,sizeof(float),fin);
    /* soil_mal */
    fread(fptr,1,sizeof(float),fin);
    /* resevap */
    fread(siptr,1,sizeof(short int),fin);
    RESEVAPDATA[i][3] = (float)fptr[0];
    /* wind */
    fread(usiptr,1,sizeof(unsigned short int),fin);
    RESEVAPDATA[i][2] = (float)usiptr[0];
    /* air_temp */
    fread(siptr,1,sizeof(short int),fin);
    RESEVAPDATA[i][1] = (float)siptr[0]/100;

  }  

  free((char *)cptr);
  free((char *)siptr);
  free((char *)usiptr);
  free((char *)iptr);
  free((char *)fptr);
  free((char *)name);

  fclose(fin);
}

