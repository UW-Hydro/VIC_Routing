/* ===============================================================
print the results (parameter set, objective function values,
and convergences to the output files

Yuqiong Liu, March  2003
================================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"
#include "stdlib.h"

int DaysOfThisMonth(int);

void Output2(double final_input[NTSTEP1][NINPUT],double final_output[NTSTEP1][NFLUX],
	     ResCar_ptr rescar_ptr) {

    FILE  *fd; //daily values
    FILE  *fm; //monthly values
    FILE  *fy; //yearly values
    int i, j;
    int years,startyear,year,month,ndays;
    float ***MONTHLY;
    float **YEARLY;
    float ResVolume;

    years=(int)NTSTEP1/365;
    MONTHLY = (float***)calloc(years+1,sizeof(float*));
    for(i=0;i<(years+1);i++) {
      MONTHLY[i] = (float**)calloc(13,sizeof(float));
      for(j=0;j<13;j++) 
	MONTHLY[i][j] = (float*)calloc(3,sizeof(float));
    }
    YEARLY = (float**)calloc(13,sizeof(float));
    for(i=0;i<13;i++) 
      YEARLY[i] = (float*)calloc(3,sizeof(float));
 
    startyear=(int)final_input[0][0];
    ResVolume=(rescar_ptr->MaxHead-rescar_ptr->MinHead)*rescar_ptr->SurfArea; //MCM

    fd = fopen("output/final.txt","w");
    for (i=0; i<NTSTEP1; i++) {
      year=(int)final_input[i][0];
      month=(int)final_input[i][1];
      ndays=DaysOfThisMonth(month);
      fprintf(fd,"%d\t%d\t%d\t%.1f\t%.1f\t%.1f\t%.3f",
	      (int)final_input[i][0],(int)final_input[i][1],(int)final_input[i][2],final_input[i][3],final_output[i][0],final_output[i][1],final_output[i][1]/ResVolume);
      fprintf(fd,"\n");
      MONTHLY[year-startyear][month][0] += final_input[i][3]/ndays;
      MONTHLY[year-startyear][month][1] += final_output[i][0]/ndays;
    }
    fclose(fd);

    fm = fopen("output/final_month.txt","w");
    for (i=0;i<years; i++) {
      for(j=1;j<=12;j++) {
	fprintf(fm,"%d\t%d\t%.2f\t%.2f",i+startyear,j,MONTHLY[i][j][0],MONTHLY[i][j][1]);
      fprintf(fm,"\n");
      YEARLY[j][0]+=MONTHLY[i][j][0]/years;
      YEARLY[j][1]+=MONTHLY[i][j][1]/years;
      }
    }
    fclose(fm);

    fy = fopen("output/final_year.txt","w");
    for(j=1;j<=12;j++) {
      fprintf(fy,"%d\t%.2f\t%.2f",j,YEARLY[j][0],YEARLY[j][1]);
      fprintf(fy,"\n");
    }
    fclose(fy);
}

int DaysOfThisMonth(int month)
{
  int DaysInMonth[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  int i;

  i=DaysInMonth[month-1];

  return i;
}
