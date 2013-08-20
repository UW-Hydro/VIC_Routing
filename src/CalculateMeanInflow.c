#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rout.h"

void CalculateMeanInflow(double *FLOW,
			 TIME *DATE,
			 int ndays,
			 float *mean_inflow,
			 float *flow_accumulated,
			 float *month_inflow)
{
  int i,j;
  int mm=0;
  int yy=0;
  int years,startyear;
  int DaysInMonth[13] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };

  years=(int)ndays/365;
  startyear=DATE[1].year;
  (*mean_inflow)=0.;
  for(i=0;i<13;i++) month_inflow[i]=0.;

  for(i=1;i<=ndays;i++) {
    (*mean_inflow)+=FLOW[i]*CONV_M3S_CM; //number in m3/day
    mm=DATE[i].month;
    yy=DATE[i].year-startyear;
    month_inflow[mm]+=FLOW[i]*CONV_M3S_CM/DaysInMonth[mm];
    flow_accumulated[i]=FLOW[i]*CONV_M3S_CM+flow_accumulated[i-1]; 
  }

  (*mean_inflow)/=ndays;
   month_inflow[0]=month_inflow[12];

  for(i=1;i<13;i++) 
    printf("month %d inflow %f (m3/month) or %f (m3/s) mean annual %f (m3/day) or %f (m3/s)\n",
	   i,month_inflow[i],month_inflow[i]/(DaysInMonth[mm]*CONV_M3S_CM),(*mean_inflow),(*mean_inflow)/CONV_M3S_CM);


}
