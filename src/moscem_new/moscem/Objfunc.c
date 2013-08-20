/*==========================================================
Call specified objective function
RMSE, Standard deviation (STD), HMLE, NSE, POW, IRR or FLOOD
or WAT
Yuqiong Liu, March 2003
===========================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void ObjFunc(Moscem *moscem,double **Output,double **ValData,
	     double *Obj, int *iter,double mean_streamflow,
	     ResCar_ptr rescar_ptr) {

     int i,j;
     double *obs,*comp,*waterdemand;
     int *month;

     obs = DoubleVector(moscem->nTSteps_Fluxes);
     comp = DoubleVector(moscem->nTSteps_Fluxes);
     waterdemand =  DoubleVector(moscem->nTSteps_Fluxes);
     month =IntVector(moscem->nTSteps_Fluxes);

     for (i=0; i<moscem->nFluxes; i++)  {  
         for (j=0; j<moscem->nTSteps_Fluxes; j++) {
             if (ValData[i][j] != MISSING_VALUE) {
	       comp[j] = Output[j][0]; //simulated outflow, m3/day
	       month[j] = (int)ValData[j][1];
	       waterdemand[j]=ValData[j][5]; //irrigation water demands
	       obs[j]= ValData[j][3]; // obs[j] initialized to inflow.

	       if(rescar_ptr->ObjectiveFunction==5 ) //POW
		 obs[j]= Output[j][1]; //power production 
	       if(rescar_ptr->ObjectiveFunction==6 ) //IRR	
  		 obs[j]=waterdemand[j]; //water demands downstream, units= m3/day
	       if(rescar_ptr->ObjectiveFunction==7 ) //FLOOD
  		 obs[j]=rescar_ptr->MeanFlood; // mean annual flood, units = m3/day
	       if(rescar_ptr->ObjectiveFunction==9 ) //WAT
		 obs[j]=mean_streamflow; //mean annual streamflow, units = m3/day
             }
         }

         if ( j == 0)  { 
	   Obj[i]=INF; 
	   printf("Attention: no validation data available for flux %2d.\n");
	   printf("Objective function value set to %e.\n", INF);
	   break;
         }

	 if(rescar_ptr->ObjectiveFunction==5) //POW
	   Obj[i] = Pow( obs, comp, moscem->nValTsteps[i]); //these functions are found in Objectives.c
 	 if(rescar_ptr->ObjectiveFunction==6) //IRR
	   Obj[i] = Irr( obs, comp, moscem->nValTsteps[i]);
 	 if(rescar_ptr->ObjectiveFunction==7) //FLOOD
	   Obj[i] = Flood( obs, comp, moscem->nValTsteps[i]);
 	 if(rescar_ptr->ObjectiveFunction==9) { //WAT
	   Obj[i] = Wat( obs, comp, moscem->nValTsteps[i]);
 
	 }
     }
     
     (*iter)++;

     FreeDoubleVector(obs);
     FreeDoubleVector(comp);
     FreeDoubleVector(waterdemand);
     FreeIntVector(month);
}
