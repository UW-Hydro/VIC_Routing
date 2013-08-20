#include <string.h>       
#include <stdio.h> 
#include <math.h> 
#include <stdlib.h> 
#include "model.h"
#include "constant.h"

#define NOTFEASIBLE 1
#define EFF 0.85     //efficiency of power generating system 
#define GE 9.81      //acceleration due to gravity, m/s2 
#define MINPROD 0.1 /*minimum power production, 
		      fraction of installed power production capacity*/

int Reservoir(double *xpar,struct OBS *obs,
	      struct OUTPUT *output, int flag,
	      float mean_streamflow, ResCar_ptr rescar_ptr, 
	      char outfilename[MAX_FNAME_LEN]) {

  FILE *fd,*fm,*fy;
  int i,j;
  int mm,dd;
  int years,startyear,year,month,day;
  int ndays;
  double storage,old_storage,temp_storage;
  double inflow,outflow,minflow,mean_flood;
  double possible_release,spill;
  float head;
  float max_storage;
  float end_storage;
  float max_release;
  float min_flow;
  float resevap;
  float waterdemand;
  float min_powerprod;
  float power_prod;
  float surfarea;
  float *demand;
  float surplus_storage;
  float outmcm;
  float **OUTPUT; 
  float *flow_accumulated;
  float *minflow_accumulated;
  float *waterdemand_accumulated; 
  float *outflow_accumulated;
  float waterdemand_mean;
  char *fday,*fmonth,*fyear;
  float temp_flowacc;
  float temp_minacc;
  float dummy;
  
  years=(int)(NTSTEP1/365);
  startyear=obs->Year[0];

  /* Initialize */
  OUTPUT = (float**)calloc(1000,sizeof(float));
  for(i=0;i<1000;i++) 
    OUTPUT[i] = (float*)calloc(6,sizeof(float));
  demand=(float*)calloc(13,sizeof(float));
  flow_accumulated=(float*)calloc(NTSTEP1+1,sizeof(float));
  minflow_accumulated=(float*)calloc(NTSTEP1+1,sizeof(float));
  waterdemand_accumulated=(float*)calloc(NTSTEP1+1,sizeof(float));
  outflow_accumulated=(float*)calloc(NTSTEP1+1,sizeof(float));

  max_storage=rescar_ptr->MaxStorage; // in m3
  storage=rescar_ptr->StartStorage;  //in m3
  end_storage=rescar_ptr->EndStorage;  //in m3
  surfarea=rescar_ptr->SurfArea; //in m2

  // numbers in m3 or m3day-1
  for(j=0;j<12;j++) 
    demand[j]=xpar[j]*mean_streamflow; 

  flow_accumulated[0]=obs->Qin[0]; /* Qin is simulated inflow. All numbers in m3 */
  minflow_accumulated[0]=obs->Qoutmin[0]; /* 7q10, m3 */
  waterdemand_accumulated[0]=obs->WaterDemand[0];
  outflow_accumulated[0]=0.;

  for(j=1;j<NTSTEP1;j++) {
    flow_accumulated[j]+=flow_accumulated[j-1]+obs->Qin[j]; /* Qin is simulated inflow. All numbers in m3 */
    minflow_accumulated[j]+=minflow_accumulated[j-1]+obs->Qoutmin[j]; /* 7q10, m3 */
    waterdemand_accumulated[j]+=waterdemand_accumulated[j-1]+obs->WaterDemand[j]; /* irrigation waterdemand, m3 */  
  }
  waterdemand_mean=waterdemand_accumulated[NTSTEP1-1]/NTSTEP1;
  
  for(i=0;i<NTSTEP1;i++) {
    mm=obs->Month[i];
    dd=obs->Day[i];
    ndays=DaysOfMonth(mm);

    storage-=obs->Resevap[i]*surfarea; // subtract today's evaporation
    old_storage=storage;
    inflow=obs->Qin[i]; /* simulated inflow, m3/day */
    minflow=obs->Qoutmin[i]; //7q10, m3/day
    waterdemand=obs->WaterDemand[i]; // Waterdemand. m3/day
    mean_flood=obs->MeanFlood[i]; // Mean annual flood. m3/day

    /* Calculate max release possible at current timestep, 
       in addition to minflow and waterdemand,
       to ensure reservoir filling will be at least
       x percent at the end of the hydrologic year */ 
    temp_flowacc=flow_accumulated[NTSTEP1-1]-flow_accumulated[i];
    temp_minacc=minflow_accumulated[NTSTEP1-1]-minflow_accumulated[i];
    if((flow_accumulated[NTSTEP1-1]-flow_accumulated[i]
	-(minflow_accumulated[NTSTEP1-1]-minflow_accumulated[i])
	-(waterdemand_accumulated[NTSTEP1-1]-waterdemand_accumulated[i]))>=end_storage)
      max_release=storage; 
    else
      max_release=storage+flow_accumulated[NTSTEP1-1]-flow_accumulated[i]
	-(minflow_accumulated[NTSTEP1-1]-minflow_accumulated[i])
	-(waterdemand_accumulated[NTSTEP1-1]-waterdemand_accumulated[i])
	-end_storage;  /* less than current storage */

    if(max_release<0) max_release=0.;

    temp_storage=old_storage+inflow;  //available water, incl today's inflow

    if(temp_storage<max_release) 
      possible_release=temp_storage;  //possible reservoir release, in addition to 7q10 and irrigation water demand
    else 
      possible_release=max_release;
    if(possible_release>mean_flood) //keep at less than mean flood - for all purposes, always!
      possible_release=mean_flood;

    /* "Demand" given in m3day-1 */
    if(possible_release<demand[mm-1]) //remember: demand[mm] = xpar[mm]*mean_streamflow
      outflow=minflow+waterdemand+possible_release;
    else
      outflow=minflow+waterdemand+demand[mm-1]; //outflow given in m3day-1
    
    if(outflow>temp_storage) outflow=temp_storage;
    
    /* Power production */
    head=rescar_ptr->MaxHead-((max_storage-storage)/surfarea); /* assuming rectangular cross-section,
                                                                       and that dam height = max head */ 
 
    if(head<0) head=0.; /* see e.g. ust-khantaika in yenisei river in the UNH dam dataset...., surface area must be wrong !!! */
    power_prod=0.;
    power_prod=outflow/CONV_M3S_CM*EFF*GE*head/1000.; //in MW
    
    if(power_prod>=(rescar_ptr->InstCap) && rescar_ptr->InstCap>0) {
      power_prod=rescar_ptr->InstCap; 
      //      outflow=power_prod*CONV_M3S_CM/(EFF*GE*head);
    }
    
    surplus_storage=0.;
    if((inflow-outflow)>(max_storage-old_storage)) 
      surplus_storage=inflow-outflow-(max_storage-old_storage);
    temp_storage=old_storage+inflow-outflow;
    storage=min(max_storage,temp_storage);
    spill=surplus_storage;
 
    outflow+=spill; 
    outflow_accumulated[i]=outflow_accumulated[i-1]+outflow;

    OUTPUT[i][0]=outflow; //outflow from reservoir, in m3day-1
    OUTPUT[i][1]=waterdemand+minflow; //waterdemand+minflow, in m3day-1
    OUTPUT[i][2]=storage/max_storage; //fraction
    OUTPUT[i][3]=power_prod; //power_prod, in MW
    OUTPUT[i][4]=spill; //spill, in m3day-1
    OUTPUT[i][5]=mean_flood; //mean annual flood, in m3day-1
  }

  if(flag==1) {  //i.e final simulation, after optimization
    /*printf("\nStorage at last time step: %f Fraction of max storage: %f MAXTSTEP %d startyear%d\n",
	   storage,storage/max_storage,MAXTSTEP,startyear);
    printf("Reservoir final: flow_accumulated:%.1f minflow_accumulated:%.1f waterdemand_accumulated:%.1f diff:%.1f (m3)\n",
	     flow_accumulated[NTSTEP1-1],minflow_accumulated[NTSTEP1-1],waterdemand_accumulated[NTSTEP1-1],flow_accumulated[NTSTEP1-1]-minflow_accumulated[NTSTEP1-1]-waterdemand_accumulated[NTSTEP1-1]);
    printf("Reservoir final: instcap:%.1f head:%.1f objfunc:%d mean_flood:%f (m3day-1) %.1f (m3s-1)\n",
	   rescar_ptr->InstCap,head,rescar_ptr->ObjectiveFunction,mean_flood,mean_flood/CONV_M3S_CM);
    printf("Reservoir final: mean streamflow:%.1f (m3day-1) %.1f (m3s-1) meandemand %f (m3day-1) %f (m3s-1)\n",
    mean_streamflow,mean_streamflow/CONV_M3S_CM,waterdemand_mean,waterdemand_mean/CONV_M3S_CM);*/
    for(i=0;i<13;i++) printf("xp %10.2f ",xpar[i]);
    printf("\n");
    for(i=0;i<12;i++) printf("ms %10.0f ",mean_streamflow);
    printf("\n");
    for(i=0;i<12;i++) printf("de %10.0f ",demand[i]);
    printf("\n");
    dummy=0;
    for(i=0;i<12;i++) {
      printf("ca %10.0f ",xpar[i]*mean_streamflow);
      dummy+=xpar[i]*mean_streamflow;
    }
    printf("\n %f %.0f %.0f %.2f\n",dummy,flow_accumulated[NTSTEP1-1],outflow_accumulated[NTSTEP1-1],flow_accumulated[NTSTEP1-1]/outflow_accumulated[NTSTEP1-1]);
  }

  if(rescar_ptr->ObjectiveFunction==5) { //Pow 
    for (i=0;i<NTSTEP1;i++) {
      output->Qcomp_total[i][0] = OUTPUT[i][0]; //outflow, m3day-1
      output->Qcomp_total[i][1] = OUTPUT[i][3]; //power_prod
    }
  }
  if(rescar_ptr->ObjectiveFunction==6) { //Irr 
    for (i=0;i<NTSTEP1;i++) {
      output->Qcomp_total[i][0] = OUTPUT[i][0]; //outflow, m3day-1
      output->Qcomp_total[i][1] = OUTPUT[i][1]; //waterdemand+minflow, m3day-1
    }
  }
  if(rescar_ptr->ObjectiveFunction==7) { //Flood 
    for (i=0;i<NTSTEP1;i++) {
      output->Qcomp_total[i][0] = OUTPUT[i][0]; //outflow, m3day-1
      output->Qcomp_total[i][1] = mean_flood; //mean annual flood, m3 day-1
    }
  }
  if(rescar_ptr->ObjectiveFunction==9) { //Wat
    for (i=0;i<NTSTEP1;i++) {
      output->Qcomp_total[i][0] = OUTPUT[i][0]; //outflow, m3day-1
      output->Qcomp_total[i][1] = mean_streamflow; //mean annual flow
    }
  }

  if(flag==1) { // Final simulation, print output file (output/output.day) read by routing program
    fd = fopen(outfilename,"w");
    for (i=0; i<NTSTEP1; i++) {
      year=obs->Year[i];
      month=obs->Month[i];
      day=obs->Day[i];
      ndays=DaysOfMonth(month);
      /* print output: year,month,day,simulated inflow to reservoir (m3/day),sim outflow from reservoir (m3/day),power_production,
	 spill,storage (m3),storage/max_storage,minflow (m3/day),waterdemand (m3/day) */
      fprintf(fd,"%d\t%d\t%d\t%10.1f %10.2f %10.1f %10.1f %10.1f %.3f %.3f %.3f\n",
	      year,month,day,obs->Qin[i],OUTPUT[i][0],OUTPUT[i][3],
	      OUTPUT[i][4],OUTPUT[i][2]*max_storage,OUTPUT[i][2],obs->Qoutmin[i],obs->WaterDemand[i]);
    }
    fclose(fd);
  }

  for(i=0;i<NTSTEP1;i++) 
    free(OUTPUT[i]);
  free(OUTPUT);
  free(demand);

  return 0;
}

