/*====================================================================
Read the flags of fluxes: "1" for optimize and "0" for non-optimize

Yuqiong Liu, March 2003
====================================================================*/

#include "constant.h"
#include "datatype.h"
#include "utility.h"

void GetObjOptFlag(char *fname, Moscem *moscem) {

     int i,n;
     FILE *fp;
     char str[MAX_LINE_LEN];	

     fp = Fopen (fname,"r");
     n = 0;
     for (i=0;i<moscem->nFluxes; i++)  {
	 fscanf(fp,"%s%d", str, &moscem->ObjOptFlag[i]);
	 if (moscem->ObjOptFlag[i] == 1) n = n+1;
     }
     if (n != moscem->nOptObj) PrintError("No. of fluxes to be optimized not consistent!");

     fclose(fp);

}
