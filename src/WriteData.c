#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"

void WriteData(double *FLOW,
	       float *R_FLOW,
	       float *STORAGE,
	       float *LEVEL,
	       float *PowerProd,
	       char *name,
	       char *outpath,
	       int ndays,
	       int nmonths,
	       TIME *DATE,
	       float factor_sum,
	       int first_year,
	       int first_month,
	       int last_year,
	       int last_month,
               int start_year,
	       float lat,
	       float lon,
	       int type,
	       int decimal_places)

{
  FILE *fp,*fp_routed,*fp_reservoir;
  char *filename_fp,*filename_fp_routed,*filename_fp_reservoir;
  char fmtstr[17];
  char LATLON[50];
  float MONTHLY[MAXYEARS+1][13];  /*variables for monthly means*/
  float YEARLY[13];
  float R_MONTHLY[MAXYEARS+1][13];
  float R_YEARLY[13];
  float S_MONTHLY[MAXYEARS+1][13];
  float S_YEARLY[13];
  float L_MONTHLY[MAXYEARS+1][13];
  float L_YEARLY[13];
  float days;
  int DaysInMonth[13] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };
  int nyears[13];
  int feb_days,feb_nrs;
  int i,j,k;
  int yy,mm;
  int leap_year;
  int count_years;

  filename_fp = (char*)calloc(100,sizeof(char));
  filename_fp_routed = (char*)calloc(100,sizeof(char));
  filename_fp_reservoir = (char*)calloc(100,sizeof(char));

  sprintf(filename_fp,"%s%s.day",outpath,name);
  if((fp = fopen(filename_fp, "w")) == NULL) {
    printf("Cannot open %s\n",filename_fp);
    exit(1);
  }

  if(type == 2 ) { //reservoir
    strcpy(filename_fp_reservoir,outpath);
    sprintf(fmtstr,"reservoir_%%.%if_%%.%if",
	    decimal_places,decimal_places);
    sprintf(LATLON,fmtstr,lat,lon);
    strcat(filename_fp_reservoir,LATLON);
    if((fp_reservoir = fopen(filename_fp_reservoir, "w")) == NULL) {
      printf("Cannot open %s\n",filename_fp_reservoir);
      exit(1);
    }
  }

  strcpy(filename_fp_routed,outpath);
  sprintf(fmtstr,"streamflow_%%.%if_%%.%if",
	  decimal_places,decimal_places);
  sprintf(LATLON,fmtstr,lat,lon);
  strcat(filename_fp_routed,LATLON);
  if((fp_routed = fopen(filename_fp_routed, "w")) == NULL) {
    printf("Cannot open %s\n",filename_fp_routed);
    exit(1);
  }
  else printf("Routed file opened: %s\n",filename_fp_routed);

  /* Initialize */
  for(i=0;i<=12;i++) {
    YEARLY[i]=0.;
    R_YEARLY[i]=0.;    
    S_YEARLY[i]=0.;
    L_YEARLY[i]=0.;    
    nyears[i]=0;
    for(j=0;j<=MAXYEARS;j++) {
      MONTHLY[j][i]=0.;
      R_MONTHLY[j][i]=0.;
      S_MONTHLY[j][i]=0.;
      L_MONTHLY[j][i]=0.;
    }
  }

  count_years=0;
  if(last_year==first_year) {
    for(i=first_month;i<=last_month;i++) 
      nyears[i]+=1;
  }
  else {
    for(i=first_month;i<=12;i++) 
      nyears[i]+=1;
    count_years++;
    for(i=1;i<=last_month;i++) 
      nyears[i]+=1;
    count_years++;
    for(k=0;k<count_years;k++) {
      for(i=1;i<=12;i++) 
	nyears[i]+=1;
    }
  }

  /* Write daily data and store monthly values */
  for(i=1;i<=ndays;i++) {
    if((DATE[i].year>first_year && DATE[i].year<last_year) 
       || (DATE[i].year==first_year && DATE[i].month>=first_month)
       || (DATE[i].year==last_year && DATE[i].month<=last_month)) {
      yy=DATE[i].year-first_year;
      mm=DATE[i].month;
      MONTHLY[yy][mm]+=FLOW[i];
      YEARLY[mm]+=FLOW[i];
      R_MONTHLY[yy][mm]+=R_FLOW[i];
      R_YEARLY[mm]+=R_FLOW[i];
      S_MONTHLY[yy][mm]+=STORAGE[i];
      S_YEARLY[mm]+=STORAGE[i];
      L_MONTHLY[yy][mm]+=LEVEL[i];
      L_YEARLY[mm]+=LEVEL[i];
      if(type==2) { /* i.e. reservoir */
	fprintf(fp,"%d %d %d %f %f %.2f %.2f\n",
		DATE[i].year,DATE[i].month,DATE[i].day,R_FLOW[i],FLOW[i],STORAGE[i],LEVEL[i]);
	fprintf(fp_reservoir,"%d %d %d %f %f %.2f %.2f %.2f\n",
		DATE[i].year,DATE[i].month,DATE[i].day,R_FLOW[i],FLOW[i],STORAGE[i],LEVEL[i],PowerProd[i]);
	fprintf(fp_routed,"%d %d %d %f\n",
		DATE[i].year,DATE[i].month,DATE[i].day,R_FLOW[i]);
      }
      else {
	fprintf(fp,"%d %d %d %f %f %.2f %.2f\n",
		DATE[i].year,DATE[i].month,DATE[i].day,FLOW[i],R_FLOW[i],0.,0.);
	fprintf(fp_routed,"%d %d %d %f\n",
		DATE[i].year,DATE[i].month,DATE[i].day,FLOW[i]);
      }
    }
  }

  fclose(fp);
  fclose(fp_routed);
  if(type==2) fclose(fp_reservoir);

  /* Write monthly data */
  sprintf(filename_fp,"%s%s.month",outpath,name);
  if((fp = fopen(filename_fp, "w")) == NULL) {
    printf("Cannot open %s\n",filename_fp);
    exit(1);
  }

  feb_days=feb_nrs=0;
  i=0;
  leap_year=IsLeapYear(i+first_year);
  if(first_year==last_year) {
    for(j=first_month;j<=last_month;j++) {
      days=DaysInMonth[j];
      if(j==2) {
	days=DaysInMonth[j]+leap_year;
	feb_days+=days;
	feb_nrs+=1;
      }
      if(type==2) {
	fprintf(fp,"%d %d %f %f %f %f\n",
		i+first_year,j,R_MONTHLY[i][j]/days,
		MONTHLY[i][j]/days,
		S_MONTHLY[i][j]/days,
		L_MONTHLY[i][j]/days);
      }
      else {
	fprintf(fp,"%d %d %f %f %f %f\n",
		i+first_year,j,MONTHLY[i][j]/days,
		R_MONTHLY[i][j]/days,
		0.,0.);
      }
    }    
  }
  else {
    for(j=first_month;j<=12;j++) {
      days=DaysInMonth[j];
      if(j==2) {
        days=DaysInMonth[j]+leap_year;
	feb_days+=days;
	feb_nrs+=1;
      }
      if(type==2) {
	fprintf(fp,"%d %d %f %f %f %f\n",
		i+first_year,j,R_MONTHLY[i][j]/days,
		MONTHLY[i][j]/days,
		S_MONTHLY[i][j]/days,
		L_MONTHLY[i][j]/days);
      }
      else {
	fprintf(fp,"%d %d %f %f %f %f\n",
		i+first_year,j,MONTHLY[i][j]/days,
		R_MONTHLY[i][j]/days,
		0.,0.);
      }
    }
    for(i=1;i<last_year-first_year;i++) {
      leap_year=IsLeapYear(i+first_year);
      for(j=1;j<=12;j++) {
	days=DaysInMonth[j];
	if(j==2) {
	  days=DaysInMonth[j]+leap_year;
	  feb_days+=days;
	  feb_nrs+=1;
	}
	if(type==2) {
	  fprintf(fp,"%d %d %f %f %f %f\n",
		  i+first_year,j,R_MONTHLY[i][j]/days,
		  MONTHLY[i][j]/days,
		  S_MONTHLY[i][j]/days,
		  L_MONTHLY[i][j]/days);
	}
	else  {
	  fprintf(fp,"%d %d %f %f %f %f\n",
		  i+first_year,j,MONTHLY[i][j]/days,
		  R_MONTHLY[i][j]/days,
		  0.,0.);
	}
      }
    }
    i=last_year-first_year;
    leap_year=IsLeapYear(i+first_year);
    for(j=1;j<=last_month;j++) {
      days=DaysInMonth[j];
      if(j==2) {
       days=DaysInMonth[j]+leap_year;
	feb_days+=days;
	feb_nrs+=1;
      }
      if(type==2) {
	fprintf(fp,"%d %d %f %f %f %f\n",
		i+first_year,j,R_MONTHLY[i][j]/days,
		MONTHLY[i][j]/days,
		S_MONTHLY[i][j]/days,
		L_MONTHLY[i][j]/days);
      }
      else {	
	fprintf(fp,"%d %d %f %f %f %f\n",
		i+first_year,j,MONTHLY[i][j]/days,
		R_MONTHLY[i][j]/days,
		0.,0.);
      }
    }   
  }
  fclose(fp);

  /* Write mean monthly data */
  sprintf(filename_fp,"%s%s.year",outpath,name);
  if((fp = fopen(filename_fp, "w")) == NULL) {
    printf("Cannot open %s\n",filename_fp);
    exit(1);
  }

  for(i=1;i<=12;i++) {
    days=DaysInMonth[i];
    if(i==2) days=(float)feb_days/(float)feb_nrs;
    if(type==2) {
      fprintf(fp,"%d %f %f %f %f\n",
	      i,R_YEARLY[i]/(nyears[i]*days),
	      YEARLY[i]/(nyears[i]*days),
	      S_YEARLY[i]/(nyears[i]*days),
	      L_YEARLY[i]/(nyears[i]*days));
    }
    else {
      fprintf(fp,"%d %f %f %f %f\n",
	      i,YEARLY[i]/(nyears[i]*days),
	      R_YEARLY[i]/(nyears[i]*days),
	      S_YEARLY[i]/(nyears[i]*days),
	      L_YEARLY[i]/(nyears[i]*days));
    }
  }

  fclose(fp);
}


