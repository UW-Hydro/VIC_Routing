/*======================== Drivemodel.c =============================
      Call the hydrological model to obtain model simulations

      Yuqiong Liu      March 2003
====================================================================*/

#include "constant.h"
#include "datatype.h"
#include "moscem.h"

void DriveModel(Moscem *moscem, double *xpar, Data_ptr data_ptr, 
		ResCar_ptr rescar_ptr,Files *files)  {

    double input[NTSTEP1][NINPUT];
    double out[NTSTEP2][NINPUT];
    float maxh,minh,surfacearea,capacity;
    int i,j;

    for (i=0;i<moscem->nTSteps_Inputs; i++) {
      for (j=0; j<moscem->nInputs; j++) input[i][j] = data_ptr->InputData[i][j]; 
        //InputData is read from infile.txt in LoadData.c
        //j=0:year,1:month,2:day,3:inflow in m3,4:q710 in m3,5:water_demand in m3,6:resevap in m3
    }
   
    maxh=rescar_ptr->MaxHead;
    minh=rescar_ptr->MinHead;
    surfacearea=rescar_ptr->SurfArea;
    capacity=rescar_ptr->InstCap;
    model(xpar, input, out, 0, data_ptr->input_mean,rescar_ptr,files->RoutOutFile);
    for (i=0;i<moscem->nTSteps_Fluxes; i++) {
      for (j=0; j<moscem->nFluxes; j++) { //nFluxes = 1 in single optimization
	data_ptr->Output[i][j] = out[i][j]; //single optimization: out[i][0]=outflow from reservoir
      }
      data_ptr->Output[i][1] = out[i][1]; //out[i][1]: Power production, in MW
    }
} 
