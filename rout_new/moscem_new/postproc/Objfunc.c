/*==========================================================
Call specified objective function
RMSE, Standard deviation (STD), HMLE, NSE, NSC, or BIAS or WAT

Yuqiong Liu, March 2003
===========================================================*/
#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "postproc.h"

void ObjFunc(Moscem *moscem,double **Output,double **ValData,double *Obj,
	     double mean_streamflow) {

     int i,j, k;
     double *obs, *comp;

     obs = DoubleVector(moscem->nTSteps_Fluxes);
     comp = DoubleVector(moscem->nTSteps_Fluxes);

     for (i=0; i<moscem->nFluxes; i++)  {
         k = 0;
         for (j=0; j<moscem->nTSteps_Fluxes; j++) {
             if (ValData[i][j] != MISSING_VALUE) {
                 obs[k] = ValData[j][i]; comp[k] = Output[j][3];
                 k++;
             }
         }

         if ( k == 0)  { 
             Obj[i]=INF; 
             printf("Attention: no validation data available for flux %2d.\n");
             printf("Objective fluction value set to %e.\n", INF);
             break;
         }

         switch(OBJ_FUNC) {
            case RMSE:
	         Obj[i] = Rmse( obs, comp, k);
                 break;
            case STD:
	         Obj[i] = Std( obs, comp, k);
                 break;
            case HMLE:
                 Obj[i] = Hmle( obs, comp, k);
                 break;
            case NSE:
                 Obj[i] = Nse( obs, comp, k);
                 break;
            case NSC:
                 Obj[i] = Nsc( obs, comp, k);
                 break;
            case BIAS:
                 Obj[i] = Bias( obs, comp, k);
                 break;
            case WAT:
                 Obj[i] = Wat( obs, comp, k,mean_streamflow);
                 break;
            default:
                 PrintError("OBJECTIVE FUNCTION NOT DEFINED");
         }
    }

    FreeDoubleVector(obs);
    FreeDoubleVector(comp);
   
}
