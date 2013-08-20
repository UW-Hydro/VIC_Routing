/*============================================================
Call the model and calculate the objective function values for
each of the samples generated from Latin-hypercube sampling

Yuqiong Liu, March 2003
============================================================*/

#include "constant.h"
#include "datatype.h"
#include "moscem.h"
#include "utility.h"

void CompObj(Moscem *moscem, Data_ptr data_ptr, Parameter_ptr par_ptr, 
	     ResCar_ptr rescar_ptr, Files *files)    {

    int i,j, control, nsteps;
    double *xpar;
    double mean_streamflow; //ingjerd

    mean_streamflow = data_ptr->input_mean;

	xpar = DoubleVector(moscem->nPars);

    for (i=0; i<moscem->nSamples; i++) {
        CompleteParsets(moscem, par_ptr, data_ptr->ParValue[i], xpar);
        control = ParControl(moscem, par_ptr, xpar);   /*check for mutual consistency */
		if (control == -1)   for(j=0; j<moscem->nFluxes; j++) data_ptr->ObjValue[i][j] = INF;
        else  {
	    DriveModel(moscem,xpar,data_ptr,rescar_ptr,files);
	    ObjFunc(moscem, data_ptr->Output,data_ptr->ValData,data_ptr->ObjValue[i], 
		    &(data_ptr->Iter),mean_streamflow,rescar_ptr);
	    //for (j=0; j<moscem->nFluxes; j++) printf("compobj objvalue %12.3E",data_ptr->ObjValue[i][j]); 
            //printf("\n");
        }
    }
    FreeDoubleVector(xpar);
}
