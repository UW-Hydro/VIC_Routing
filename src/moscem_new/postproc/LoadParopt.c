/*------------------------------------------------------
   Read in an optimal parameter set

   Yuqiong Liu, January 2003
-------------------------------------------------------*/

#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"

void LoadParopt(Moscem *moscem, Files *files, double *xpar,double *obj) 
{
 
    int i, tmp;
    FILE *fp1, *fp2;
    char str[MAX_LINE_LEN];

    fp1 = Fopen(files->ParOptFile, "r");
    fp2 = Fopen(files->ObjFile, "r");

    //printf("\nLoadParOpt:%s\n",files->ParOptFile);    
    //printf("LoadParOpt:%s %d\n",files->ObjFile,moscem->nParIdx-1);    

    for (i=0; i<moscem->nParIdx-1; i++)   {
        fgets(str, MAX_LINE_LEN, fp1); fgets(str, MAX_LINE_LEN, fp2);
	}

    fscanf(fp1, "%d", &tmp); printf(" %d ",tmp);
    for (i=0; i<moscem->nPars; i++) {
      fscanf(fp1,"%lf", &xpar[i]);
      printf(" %.3lf ",xpar[i]);
    }
    printf("\n",xpar[i]);

    fscanf(fp2, "%d", &tmp);
    for (i=0; i<moscem->nFluxes; i++) {
      fscanf(fp2,"%lf",&obj[i]);
      printf(" %lf ",xpar[i]);
    }
    printf("\n",xpar[i]);

    fclose (fp1);
    fclose (fp2);
}
