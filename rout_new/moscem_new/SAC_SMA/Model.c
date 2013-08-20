#include <math.h> 
#include "model.h"


void model(double *xpar, double input[NTSTEP1][NINPUT], double out[NTSTEP2][NINPUT], 
	   int flag, float mean_streamflow, ResCar_ptr rescar_ptr,
	   char outfilename[MAX_FNAME_LEN]) {

     int i, j;
     struct OUTPUT output;
     struct OBS obs;
     double tmp;
     int tw;

     tw = NTSTEP1-NTSTEP2;    /* steps of warm-up period */

    for (i=0; i<NTSTEP1; i++) { // input read in LoadData.c
 	obs.Year[i] = (int)input[i][0];      
	obs.Month[i] = (int)input[i][1];
	obs.Day[i] = (int)input[i][2];	
	obs.Qin[i] = input[i][3]; /* simulated inflow to reservoir in m3/day */
	obs.Qoutmin[i] = input[i][4]; /* min release, i.e 7q10 in m3/day */
	obs.WaterDemand[i] = input[i][5]; /* Irrigation water demand in m3/day */
	obs.Resevap[i] = input[i][6]; /* ResEvap in m3/day */
	obs.MeanFlood[i] = input[i][7]; /* Mean annual flood in m3/day */
     }

     obs.datalength = NTSTEP1;
	 //printf("sac-sma/Model.c B %d %d %d\n",NTSTEP1,NTSTEP2,tw);
     Reservoir(xpar,&obs,&output,flag,mean_streamflow,rescar_ptr,outfilename); /* CALL to reservoir */
 	 //printf("sac-sma/Model.c C %d %d %d\n",NTSTEP1,NTSTEP2,tw);
     for (i=0;i<NTSTEP1;i++) {
       out[i][0] = output.Qcomp_total[i][0];
       out[i][1] = output.Qcomp_total[i][1];
     }
     
}
