/* ======================== ParameterInit.c ======================
	Get default values, lower and upper boundaries of parameters	

        Yuqiong Liu, March 2003
================================================================ */

#include <string.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"

void ParameterInit(char *fname,char *fname2,Moscem *moscem, Parameter_ptr parameter_ptr, ResCar_ptr rescar_ptr) {

    int i,n,dummy;
    FILE *fp;
    char str[MAX_LINE_LEN];

    fp = Fopen(fname, "r");
    fgets(str, MAX_LINE_LEN, fp);
    printf("%s",str);
    n=0;
    for(i=0; i<moscem->nPars; i++) {
	fscanf(fp,"%d%s%lf%lf%lf%d",&(parameter_ptr->ParNumber[i]),parameter_ptr->ParName[i],
	    &(parameter_ptr->ParDefault[i]), &(parameter_ptr->LowerBound[n]),
	    &(parameter_ptr->UpperBound[n]), &(parameter_ptr->OptFlag[i]));
	
        fgets(str, PAR_LNAME_LEN,fp);
	if (str[strlen(str)-1] == '\n') 
	    strncpy(parameter_ptr->ParLongName[i],str,strlen(str)-1);
	else
	    strcpy(parameter_ptr->ParLongName[i],str);
	if(parameter_ptr->OptFlag[i]==1) n=n+1; /* "1" for optimization and "0" for otherwise*/

	printf("%4d%10s%12.3e%12.3e%12.3e%4d%s\n",parameter_ptr->ParNumber[i],
	    parameter_ptr->ParName[i], parameter_ptr->ParDefault[i],
	    parameter_ptr->LowerBound[n-1],parameter_ptr->UpperBound[n-1],
	    parameter_ptr->OptFlag[i],parameter_ptr->ParLongName[i]);
    }
    if(n!=moscem->nOptPar) PrintError("No. of parameters to be optimized not consistent!");

    fclose(fp);

    /* Open Reservoir Characteristics file (data/rescarfile.txt)*/
    fp = Fopen(fname2, "r");
    fscanf(fp,"%f %f %f %f %f %f %f %f %d",&(rescar_ptr->SurfArea),&(rescar_ptr->MaxStorage),
	   &(rescar_ptr->StartStorage),&(rescar_ptr->EndStorage),&(rescar_ptr->MaxHead),&(rescar_ptr->MinHead),
	   &(rescar_ptr->InstCap),&(rescar_ptr->MeanFlood),&(rescar_ptr->ObjectiveFunction));
    printf("Parameterinit SurfArea:%.1f m3 MaxHead:%.1f m MaxProd:%.1f Capacity%.1f m3 StartStorage:%.1f m3 Endstorage%.1f m3 MeanFlood%.1f m3day-1 ObjectiveFunction %d\n",
	   rescar_ptr->SurfArea,rescar_ptr->MaxHead,rescar_ptr->InstCap,
	   rescar_ptr->MaxStorage,rescar_ptr->StartStorage,rescar_ptr->EndStorage,
	   rescar_ptr->MeanFlood,rescar_ptr->ObjectiveFunction);
    fclose(fp);
}
