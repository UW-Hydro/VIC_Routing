/*========================================================
   Define objective functinos
   RMSE, Standard deviation (STD), HMLE, NSE, NSC, or IRR

   Yuqiong Liu, March 2003
========================================================*/

#include <math.h>

#define Transform(m, n) ( (n) == 0.0 ? (log(m)) : ((pow(m,n)-1.0)/m) )
#ifndef LAMBDA
#define LAMBDA 0.5
#endif

double Rmse (double *obs, double *comp,int npts)  {
 
     int i;
     double sum = 0.0;
 
     for (i=0;i<npts;i++) 
          sum += pow(obs[i] - comp[i], 2.0);
     
     sum = pow((sum/npts), 0.5);

     return sum;
}


double Std (double *obs, double *comp,int npts)  {

     int i;
     double sum = 0.0, ave = 0.0; 

     for (i=0;i<npts;i++)  ave = ave + (comp[i]-obs[i])/npts;
     for (i=0;i<npts;i++) 
          sum += pow(comp[i]-obs[i]-ave, 2.0);
     if (npts >=1 )    sum = pow(sum/(npts-1), 0.5);
     else  sum = pow(sum/npts, 0.5);

     return sum;
}


double Hmle (double *obs, double *comp,int npts)  {     
     int i;
     double sum = 0.0;
          
     for (i=0;i<npts;i++) {
          sum += pow(Transform(obs[i],LAMBDA) - 
	       Transform(comp[i],LAMBDA), 2.);
     }  
     
     sum = pow((sum/npts), 0.5);

     return sum;
}


double Nse (double *obs, double *comp,int npts) 
{
     
     int i;
     double SRMSEC = 0.0;
     double SRMSEO = 0.0;
     double OBS_AVE = 0.0;
     
     for (i=0;i<npts;i++)  OBS_AVE +=  obs[i]/npts;
     
     for (i=0;i<npts;i++) {
	  SRMSEC += pow(obs[i] - comp[i],2.);
	  SRMSEO += pow(obs[i] - OBS_AVE,2.);
     }  
     
     return SRMSEC/SRMSEO;
}

/* Pow: obs is power production */
double Pow (double *obs, double *comp,int npts) {

     int i;
     double tot_prod=0.;
     
     for (i=0;i<npts;i++) {
       tot_prod += obs[i];
     }

     return npts*10e5/tot_prod; /*multiply with 10e5 to get a somewhat higher number.*/
}

/* Flood: comp is routed flow out of reservoir.
   obs[k] is mean annual flood (same value for all time steps */
double Flood (double *obs,double *comp,int npts,int objective) {

     int i;
     double sumsq = 0.0;

     for (i=0;i<npts;i++) {
	 if(comp[i]>obs[i]) 
	   sumsq += (comp[i] - obs[i])*(comp[i] - obs[i]);
      }  

     return sumsq;
}

/* Irr: Comp is routed flow out of reservoir.
   obs is water demand downstream of reservoir */
double Irr(double *obs,double *comp,int npts,int objective) {
  
     int i;
     double sum = 0.0;
     double irr = 0.0;

     for (i=0;i<npts;i++) {
       if(comp[i]<obs[i]) {
	 sum += obs[i]-comp[i];
       }  
     }
     irr = sum;

     return irr;
}

/* Wat: Comp is routed flow out of reservoir.
   obs is mean annual simulated inflow (same values for all time steps) */
double Wat (double *obs, double *comp,int npts,int objective) {
  
     int i;
     double sum = 0.0;
     double water = 0.0;

     for (i=0;i<npts;i++) {
	 sum += fabs(comp[i] - obs[i]);
      }  

     water = sum;

     return water;
}


