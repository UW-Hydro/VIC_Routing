/*======================== Drivemodel.c =============================
      Call the hydrological model to obtain model simulations

      Yuqiong Liu      March 2003
====================================================================*/

#include "constant.h"
#include "datatype.h"
#include "postproc.h"

void DriveModel(Moscem *moscem, double *xpar, Data_ptr data_ptr)  {

    double input[NTSTEP1][NINPUT];
    double out[NTSTEP2][NFLUX];
    int i,j;

    for (i=0;i<moscem->nTSteps_Inputs; i++) {
        for (j=0; j<moscem->nInputs; j++) input[i][j] = data_ptr->InputData[i][j];
    }

    model(xpar, input, out);

    for (i=0;i<moscem->nTSteps_Fluxes; i++)
        for (j=0; j<moscem->nFluxes; j++) data_ptr->Output[i][j] = out[i][j];
  
} 
