/*========================================================
   Define objective functinos
   RMSE, Standard deviation (STD), HMLE, NSE, NSC, or BIAS

   Yuqiong Liu, March 2003
========================================================*/

#include <math.h>
#include "constant.h"

#define Transform(m, n) ( (n) == 0.0 ? (log(m)) : ((pow(m,n)-1.0)/m) )

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

double Nsc (double *obs, double *comp,int npts) {
     int i;
     int insc = 0;
     double eprev,e;
     
     eprev = comp[0] - obs[0];
     
     for (i=1;i<npts;i++) {
	  e = comp[i] - obs[i];
	  insc += ((e*eprev)<0);
	  eprev = e;
     }
     insc = insc *(-1);
     
     return insc;
}

double Bias (double *obs, double *comp,int npts) {
  
     int i;
     double SE = 0.0;
     double bias = 0.0;
     
     for (i=0;i<npts;i++) {
	  SE += obs[i] - comp[i];
     }  
      
     bias = SE/npts;
     
     return bias;
}

double Wat (double *obs, double *comp,int npts,double mean_streamflow) {
  
     int i;
     double sumsq = 0.0;
     double water = 0.0;
     
     for (i=0;i<npts;i++) {
	  sumsq += (comp[i] - mean_streamflow)*(comp[i] - mean_streamflow);
     }  
     printf("Wat i:%d npts:%d obs:%f mean:%f sum:%f\n",i,npts,comp[0],mean_streamflow,sumsq);
     water = 100000000./sumsq;
     
     return water;
}
